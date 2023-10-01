// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::rc::Rc;

use anyhow::anyhow;
use anyhow::Result;

use crate::swc::ast::Program;
use crate::swc::codegen::text_writer::JsWriter;
use crate::swc::codegen::Node;
use crate::swc::common::chain;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::errors::Diagnostic as SwcDiagnostic;
use crate::swc::common::FileName;
use crate::swc::common::Globals;
use crate::swc::common::Mark;
use crate::swc::common::SourceMap;
use crate::swc::parser::error::SyntaxError;
use crate::swc::transforms::fixer;
use crate::swc::transforms::helpers;
use crate::swc::transforms::hygiene;
use crate::swc::transforms::pass::Optional;
use crate::swc::transforms::proposal;
use crate::swc::transforms::react;
use crate::swc::transforms::resolver;
use crate::swc::transforms::typescript;
use crate::swc::transforms::typescript::TsEnumConfig;
use crate::swc::visit::FoldWith;
use crate::Diagnostic;
use crate::DiagnosticsError;
use crate::ModuleSpecifier;
use crate::ParsedSource;
use crate::ES_VERSION;

use std::cell::RefCell;

mod transforms;

#[derive(Debug, Clone, Hash)]
pub enum ImportsNotUsedAsValues {
  Remove,
  Preserve,
  Error,
}

/// Options which can be adjusted when transpiling a module.
///
/// This implements `Hash` so the CLI can use it to bust the emit cache.
#[derive(Debug, Clone, Hash)]
pub struct EmitOptions {
  /// When emitting a legacy decorator, also emit experimental decorator meta
  /// data.  Defaults to `false`.
  pub emit_metadata: bool,
  /// What to do with import statements that only import types i.e. whether to
  /// remove them (`Remove`), keep them as side-effect imports (`Preserve`)
  /// or error (`Error`). Defaults to `Remove`.
  pub imports_not_used_as_values: ImportsNotUsedAsValues,
  /// Should the source map be inlined in the emitted code file, or provided
  /// as a separate file.  Defaults to `true`.
  pub inline_source_map: bool,
  /// Should the sources be inlined in the source map.  Defaults to `true`.
  pub inline_sources: bool,
  /// `true` if the program should use an implicit JSX import source/the "new"
  /// JSX transforms.
  pub jsx_automatic: bool,
  /// If JSX is automatic, if it is in development mode, meaning that it should
  /// import `jsx-dev-runtime` and transform JSX using `jsxDEV` import from the
  /// JSX import source as well as provide additional debug information to the
  /// JSX factory.
  pub jsx_development: bool,
  /// When transforming JSX, what value should be used for the JSX factory.
  /// Defaults to `React.createElement`.
  pub jsx_factory: String,
  /// When transforming JSX, what value should be used for the JSX fragment
  /// factory.  Defaults to `React.Fragment`.
  pub jsx_fragment_factory: String,
  /// The string module specifier to implicitly import JSX factories from when
  /// transpiling JSX.
  pub jsx_import_source: Option<String>,
  /// Should a corresponding .map file be created for the output. This should be
  /// false if inline_source_map is true. Defaults to `false`.
  pub source_map: bool,
  /// Should JSX be transformed. Defaults to `true`.
  pub transform_jsx: bool,
  /// Should import declarations be transformed to variable declarations using
  /// a dynamic import. This is useful for import & export declaration support
  /// in script contexts such as the Deno REPL.  Defaults to `false`.
  pub var_decl_imports: bool,
}

impl Default for EmitOptions {
  fn default() -> Self {
    EmitOptions {
      emit_metadata: false,
      imports_not_used_as_values: ImportsNotUsedAsValues::Remove,
      inline_source_map: true,
      inline_sources: true,
      source_map: false,
      jsx_automatic: false,
      jsx_development: false,
      jsx_factory: "React.createElement".into(),
      jsx_fragment_factory: "React.Fragment".into(),
      jsx_import_source: None,
      transform_jsx: true,
      var_decl_imports: false,
    }
  }
}

impl EmitOptions {
  fn as_typescript_strip_config(&self) -> typescript::strip::Config {
    #![allow(deprecated)]
    typescript::strip::Config {
      pragma: Some(self.jsx_factory.clone()),
      pragma_frag: Some(self.jsx_fragment_factory.clone()),
      import_not_used_as_values: match self.imports_not_used_as_values {
        ImportsNotUsedAsValues::Remove => {
          typescript::strip::ImportsNotUsedAsValues::Remove
        }
        ImportsNotUsedAsValues::Preserve => {
          typescript::strip::ImportsNotUsedAsValues::Preserve
        }
        // `Error` only affects the type-checking stage. Fall back to `Remove` here.
        ImportsNotUsedAsValues::Error => {
          typescript::strip::ImportsNotUsedAsValues::Remove
        }
      },
      // this property is deprecated, but we don't use it anyway because we target >ES2020
      use_define_for_class_fields: true,
      // no need for this to be false because we treat all files as modules
      no_empty_export: true,
      ts_enum_config: TsEnumConfig {
        treat_const_enum_as_enum: false,
        ts_enum_is_readonly: false,
      },
      // we don't suport this, so leave it as-is so it errors in v8
      import_export_assign_config:
        typescript::TsImportExportAssignConfig::Preserve,
    }
  }
}

/// Implements a configuration trait for source maps that reflects the logic
/// to embed sources in the source map or not.
#[derive(Debug)]
pub struct SourceMapConfig {
  pub inline_sources: bool,
}

impl crate::swc::common::source_map::SourceMapGenConfig for SourceMapConfig {
  fn file_name_to_source(&self, f: &FileName) -> String {
    f.to_string()
  }

  fn inline_sources_content(&self, f: &FileName) -> bool {
    match f {
      FileName::Real(..) | FileName::Custom(..) => false,
      FileName::Url(..) => self.inline_sources,
      _ => true,
    }
  }
}

/// Source transpiled based on the emit options.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct TranspiledSource {
  /// Transpiled text.
  pub text: String,
  /// Source map back to the original file.
  pub source_map: Option<String>,
}

impl ParsedSource {
  /// Transform a TypeScript file into a JavaScript file.
  pub fn transpile(&self, options: &EmitOptions) -> Result<TranspiledSource> {
    let program = (*self.program()).clone();
    let source_map = Rc::new(SourceMap::default());
    let source_map_config = SourceMapConfig {
      inline_sources: options.inline_sources,
    };
    let file_name = match ModuleSpecifier::parse(self.specifier()) {
      Ok(specifier) => FileName::Url(specifier),
      Err(_) => FileName::Custom(self.specifier().to_string()),
    };
    source_map.new_source_file(file_name, self.text_info().text().to_string());
    // needs to align with what's done internally in source map
    assert_eq!(1, self.text_info().range().start.as_byte_pos().0);
    // we need the comments to be mutable, so make it single threaded
    let comments = self.comments().as_single_threaded();
    let globals = Globals::new();
    crate::swc::common::GLOBALS.set(&globals, || {
      let top_level_mark = Mark::fresh(Mark::root());
      let program = fold_program(
        program,
        options,
        source_map.clone(),
        &comments,
        top_level_mark,
        self.diagnostics(),
      )?;

      let mut src_map_buf = vec![];
      let mut buf = vec![];
      {
        let mut writer = Box::new(JsWriter::new(
          source_map.clone(),
          "\n",
          &mut buf,
          Some(&mut src_map_buf),
        ));
        writer.set_indent_str("  "); // two spaces
        let config = crate::swc::codegen::Config {
          minify: false,
          ascii_only: false,
          omit_last_semi: false,
          target: ES_VERSION,
        };
        let mut emitter = crate::swc::codegen::Emitter {
          cfg: config,
          comments: Some(&comments),
          cm: source_map.clone(),
          wr: writer,
        };
        program.emit_with(&mut emitter)?;
      }
      let mut src = String::from_utf8(buf)?;
      let mut map: Option<String> = None;
      {
        let mut buf = Vec::new();
        source_map
          .build_source_map_with_config(&src_map_buf, None, source_map_config)
          .to_writer(&mut buf)?;

        if options.inline_source_map {
          src.push_str("//# sourceMappingURL=data:application/json;base64,");
          base64::encode_config_buf(
            buf,
            base64::Config::new(base64::CharacterSet::Standard, true),
            &mut src,
          );
        } else {
          map = Some(String::from_utf8(buf)?);
        }
      }
      Ok(TranspiledSource {
        text: src,
        source_map: map,
      })
    })
  }
}

#[derive(Default, Clone)]
struct DiagnosticCollector {
  diagnostics_cell: Rc<RefCell<Vec<SwcDiagnostic>>>,
}

impl DiagnosticCollector {
  pub fn into_handler(self) -> crate::swc::common::errors::Handler {
    crate::swc::common::errors::Handler::with_emitter(
      true,
      false,
      Box::new(self),
    )
  }
}

impl crate::swc::common::errors::Emitter for DiagnosticCollector {
  fn emit(&mut self, db: &crate::swc::common::errors::DiagnosticBuilder<'_>) {
    use std::ops::Deref;
    self.diagnostics_cell.borrow_mut().push(db.deref().clone());
  }
}

/// Low level function for transpiling a program.
pub fn fold_program(
  program: Program,
  options: &EmitOptions,
  source_map: Rc<SourceMap>,
  comments: &SingleThreadedComments,
  top_level_mark: Mark,
  diagnostics: &[Diagnostic],
) -> Result<Program> {
  ensure_no_fatal_diagnostics(diagnostics)?;

  let unresolved_mark = Mark::new();
  let mut passes = chain!(
    Optional::new(transforms::StripExportsFolder, options.var_decl_imports),
    resolver(unresolved_mark, top_level_mark, true),
    proposal::decorators::decorators(proposal::decorators::Config {
      legacy: true,
      emit_metadata: options.emit_metadata,
      use_define_for_class_fields: true,
    }),
    proposal::explicit_resource_management::explicit_resource_management(),
    helpers::inject_helpers(top_level_mark),
    Optional::new(
      typescript::strip::strip_with_config(
        options.as_typescript_strip_config(),
        top_level_mark
      ),
      !options.transform_jsx
    ),
    Optional::new(
      typescript::strip::strip_with_jsx(
        source_map.clone(),
        options.as_typescript_strip_config(),
        comments,
        top_level_mark
      ),
      options.transform_jsx
    ),
    Optional::new(
      react::react(
        source_map.clone(),
        Some(comments),
        #[allow(deprecated)]
        react::Options {
          pragma: Some(options.jsx_factory.clone()),
          pragma_frag: Some(options.jsx_fragment_factory.clone()),
          // This will use `Object.assign()` instead of the `_extends` helper
          // when spreading props (Note: this property is deprecated)
          use_builtins: Some(true),
          runtime: if options.jsx_automatic {
            Some(react::Runtime::Automatic)
          } else {
            None
          },
          development: Some(options.jsx_development),
          import_source: Some(
            options.jsx_import_source.clone().unwrap_or_default()
          ),
          next: None,
          refresh: None,
          throw_if_namespace: Some(false),
          use_spread: None,
        },
        top_level_mark,
        unresolved_mark,
      ),
      options.transform_jsx
    ),
    Optional::new(
      transforms::ImportDeclsToVarDeclsFolder,
      options.var_decl_imports
    ),
    fixer(Some(comments)),
    hygiene(),
  );

  let emitter = DiagnosticCollector::default();
  let diagnostics_cell = emitter.diagnostics_cell.clone();
  let handler = emitter.into_handler();
  let result = crate::swc::common::errors::HANDLER.set(&handler, || {
    helpers::HELPERS.set(&helpers::Helpers::new(false), || {
      program.fold_with(&mut passes)
    })
  });

  let diagnostics = diagnostics_cell.borrow();
  ensure_no_fatal_swc_diagnostics(&source_map, diagnostics.iter())?;
  Ok(result)
}

fn ensure_no_fatal_swc_diagnostics<'a>(
  source_map: &SourceMap,
  diagnostics: impl Iterator<Item = &'a SwcDiagnostic>,
) -> Result<()> {
  let fatal_diagnostics = diagnostics
    .filter(|d| is_fatal_swc_diagnostic(d))
    .collect::<Vec<_>>();
  if !fatal_diagnostics.is_empty() {
    Err(anyhow!(
      "{}",
      fatal_diagnostics
        .iter()
        .map(|d| format_swc_diagnostic(source_map, d))
        .collect::<Vec<_>>()
        .join("\n\n")
    ))
  } else {
    Ok(())
  }
}

fn is_fatal_swc_diagnostic(diagnostic: &SwcDiagnostic) -> bool {
  use crate::swc::common::errors::Level;
  match diagnostic.level {
    Level::Bug
    | Level::Cancelled
    | Level::FailureNote
    | Level::Fatal
    | Level::PhaseFatal
    | Level::Error => true,
    Level::Help | Level::Note | Level::Warning => false,
  }
}

fn format_swc_diagnostic(
  source_map: &SourceMap,
  diagnostic: &SwcDiagnostic,
) -> String {
  if let Some(span) = &diagnostic.span.primary_span() {
    let file_name = source_map.span_to_filename(*span);
    let loc = source_map.lookup_char_pos(span.lo);
    format!(
      "{} at {}:{}:{}",
      diagnostic.message(),
      file_name,
      loc.line,
      loc.col_display + 1,
    )
  } else {
    diagnostic.message()
  }
}

fn ensure_no_fatal_diagnostics(
  diagnostics: &[Diagnostic],
) -> Result<(), DiagnosticsError> {
  let fatal_diagnostics = diagnostics
    .iter()
    .filter(|d| is_fatal_syntax_error(&d.kind))
    .map(ToOwned::to_owned)
    .collect::<Vec<_>>();
  if !fatal_diagnostics.is_empty() {
    Err(DiagnosticsError(fatal_diagnostics))
  } else {
    Ok(())
  }
}

fn is_fatal_syntax_error(error_kind: &SyntaxError) -> bool {
  matches!(
    error_kind,
    // expected identifier
    SyntaxError::TS1003 |
        // expected semi-colon
        SyntaxError::TS1005 |
        // octal literals not allowed
        SyntaxError::TS1085 |
        SyntaxError::LegacyOctal |
        SyntaxError::LegacyDecimal |
        // expected expression
        SyntaxError::TS1109 |
        // unterminated string literal
        SyntaxError::UnterminatedStrLit |
        // nullish coalescing with logical op
        SyntaxError::NullishCoalescingWithLogicalOp |
        // missing a token
        SyntaxError::Expected(_, _)
  )
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::parse_module;
  use crate::MediaType;
  use crate::ParseParams;
  use crate::SourceTextInfo;

  use pretty_assertions::assert_eq;

  #[test]
  fn test_transpile() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
enum D {
  A,
  B,
}

namespace N {
  export enum D {
    A = "value"
  }
  export const Value = 5;
}

export class A {
  private b: string;
  protected c: number = 1;
  e: "foo";
  constructor (public d = D.A) {
    const e = "foo" as const;
    this.e = e;
    console.log(N.Value);
  }
}
    "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = module.transpile(&EmitOptions::default()).unwrap();
    let expected_text = r#"var D;
(function(D) {
  D[D["A"] = 0] = "A";
  D[D["B"] = 1] = "B";
})(D || (D = {}));
var N;
(function(N) {
  let D;
  (function(D) {
    D["A"] = "value";
  })(D = N.D || (N.D = {}));
  var Value = N.Value = 5;
})(N || (N = {}));
export class A {
  d;
  b;
  c;
  e;
  constructor(d = D.A){
    this.d = d;
    this.c = 1;
    const e = "foo";
    this.e = e;
    console.log(N.Value);
  }
}
"#;
    assert_eq!(
      &transpiled_source.text[..expected_text.len()],
      expected_text
    );
    assert!(transpiled_source
      .text
      .contains("\n//# sourceMappingURL=data:application/json;base64,"));
    assert!(transpiled_source.source_map.is_none());
  }

  #[test]
  fn test_explicit_resource_management() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = "using data = create();\nconsole.log(data);";
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = module.transpile(&EmitOptions::default()).unwrap();
    let expected_text = r#"function dispose_SuppressedError(suppressed, error) {
  if (typeof SuppressedError !== "undefined") {
    dispose_SuppressedError = SuppressedError;
  } else {
    dispose_SuppressedError = function SuppressedError(suppressed, error) {
      this.suppressed = suppressed;
      this.error = error;
      this.stack = new Error().stack;
    };
    dispose_SuppressedError.prototype = Object.create(Error.prototype, {
      constructor: {
        value: dispose_SuppressedError,
        writable: true,
        configurable: true
      }
    });
  }
  return new dispose_SuppressedError(suppressed, error);
}
function _dispose(stack, error, hasError) {
  function next() {
    while(stack.length > 0){
      try {
        var r = stack.pop();
        var p = r.d.call(r.v);
        if (r.a) return Promise.resolve(p).then(next, err);
      } catch (e) {
        return err(e);
      }
    }
    if (hasError) throw error;
  }
  function err(e) {
    error = hasError ? new dispose_SuppressedError(e, error) : e;
    hasError = true;
    return next();
  }
  return next();
}
function _using(stack, value, isAwait) {
  if (value === null || value === void 0) return value;
  if (typeof value !== "object") {
    throw new TypeError("using declarations can only be used with objects, null, or undefined.");
  }
  if (isAwait) {
    var dispose = value[Symbol.asyncDispose || Symbol.for("Symbol.asyncDispose")];
  }
  if (dispose === null || dispose === void 0) {
    dispose = value[Symbol.dispose || Symbol.for("Symbol.dispose")];
  }
  if (typeof dispose !== "function") {
    throw new TypeError(`Property [Symbol.dispose] is not a function.`);
  }
  stack.push({
    v: value,
    d: dispose,
    a: isAwait
  });
  return value;
}
try {
  var _stack = [];
  var data = _using(_stack, create());
  console.log(data);
} catch (_) {
  var _error = _;
  var _hasError = true;
} finally{
  _dispose(_stack, _error, _hasError);
}"#;
    assert_eq!(
      &transpiled_source.text[..expected_text.len()],
      expected_text
    );
  }

  #[test]
  fn test_transpile_tsx() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
    export class A {
      render() {
        return <div><span></span></div>
      }
    }
    "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = module.transpile(&EmitOptions::default()).unwrap();
    assert!(transpiled_source
      .text
      .contains("React.createElement(\"div\", null"));
  }

  #[test]
  fn test_transpile_tsx_with_namespace() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
    export class A {
      render() {
        return <my:tag><span my:attr="this"></span></my:tag>
      }
    }
    "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = module.transpile(&EmitOptions::default()).unwrap();
    assert!(transpiled_source
      .text
      .contains("React.createElement(\"my:tag\", null"));
    assert!(transpiled_source.text.contains("\"my:attr\": \"this\""));
  }

  #[test]
  fn test_transpile_jsx_pragma() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
/** @jsx h */
/** @jsxFrag Fragment */
import { h, Fragment } from "https://deno.land/x/mod.ts";

function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    let expected = r#"/** @jsx h */ /** @jsxFrag Fragment */ import { h, Fragment } from "https://deno.land/x/mod.ts";
function App() {
  return /*#__PURE__*/ h("div", null, /*#__PURE__*/ h(Fragment, null));
}"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_pragma() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
/** @jsxImportSource jsx_lib */

function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    let expected = r#"/** @jsxImportSource jsx_lib */ import { jsx as _jsx, Fragment as _Fragment } from "jsx_lib/jsx-runtime";
function App() {
  return /*#__PURE__*/ _jsx("div", {
    children: /*#__PURE__*/ _jsx(_Fragment, {})
  });
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_no_pragma() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let emit_options = EmitOptions {
      jsx_automatic: true,
      jsx_import_source: Some("jsx_lib".to_string()),
      ..Default::default()
    };
    let code = module.transpile(&emit_options).unwrap().text;
    let expected = r#"import { jsx as _jsx, Fragment as _Fragment } from "jsx_lib/jsx-runtime";
function App() {
  return /*#__PURE__*/ _jsx("div", {
    children: /*#__PURE__*/ _jsx(_Fragment, {})
  });
}
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_no_pragma_dev() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let emit_options = EmitOptions {
      jsx_automatic: true,
      jsx_import_source: Some("jsx_lib".to_string()),
      jsx_development: true,
      ..Default::default()
    };
    let code = module.transpile(&emit_options).unwrap().text;
    let expected = r#"import { jsxDEV as _jsxDEV, Fragment as _Fragment } from "jsx_lib/jsx-dev-runtime";
function App() {
  return /*#__PURE__*/ _jsxDEV("div", {
    children: /*#__PURE__*/ _jsxDEV(_Fragment, {}, void 0, false)
  }, void 0, false, {
    fileName: "https://deno.land/x/mod.tsx",
    lineNumber: 3,
    columnNumber: 5
  }, this);
}
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_pragma_var_decl_imports() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
/** @jsxImportSource jsx_lib */

function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let emit_options = EmitOptions {
      var_decl_imports: true,
      ..Default::default()
    };
    let code = module.transpile(&emit_options).unwrap().text;
    let expected = r#"/** @jsxImportSource jsx_lib */ const { "jsx": _jsx1, "Fragment": _Fragment1 } = await import("jsx_lib/jsx-runtime");
function App() {
  return /*#__PURE__*/ _jsx("div", {
    children: /*#__PURE__*/ _jsx(_Fragment, {})
  });
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_decorators() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
    function enumerable(value: boolean) {
      return function (
        _target: any,
        _propertyKey: string,
        descriptor: PropertyDescriptor,
      ) {
        descriptor.enumerable = value;
      };
    }

    export class A {
      @enumerable(false)
      a() {
        Test.value;
      }
    }
    "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    let expected = r#"function _ts_decorate(decorators, target, key, desc) {
  var c = arguments.length, r = c < 3 ? target : desc === null ? desc = Object.getOwnPropertyDescriptor(target, key) : desc, d;
  if (typeof Reflect === "object" && typeof Reflect.decorate === "function") r = Reflect.decorate(decorators, target, key, desc);
  else for(var i = decorators.length - 1; i >= 0; i--)if (d = decorators[i]) r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
  return c > 3 && r && Object.defineProperty(target, key, r), r;
}
function enumerable(value) {
  return function(_target, _propertyKey, descriptor) {
    descriptor.enumerable = value;
  };
}
export class A {
  a() {
    Test.value;
  }
}
_ts_decorate([
  enumerable(false)
], A.prototype, "a", null);"#;
    assert_eq!(&code[0..expected.len()], expected);
  }

  #[test]
  fn transpile_handle_code_nested_in_ts_nodes_with_jsx_pass() {
    // from issue 12409
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
export function g() {
  let algorithm: any
  algorithm = {}

  return <Promise>(
    test(algorithm, false, keyUsages)
  )
}
  "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_options = EmitOptions {
      transform_jsx: true,
      ..Default::default()
    };
    let code = module.transpile(&emit_options).unwrap().text;
    let expected = r#"export function g() {
  let algorithm;
  algorithm = {};
  return test(algorithm, false, keyUsages);
}"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn transpile_bitshift_typescript() {
    // from https://github.com/denoland/deno/issues/14900
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
for (let i = 0; i < testVariable >> 1; i++) callCount++;
  "#;
    let module = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module.transpile(&Default::default()).unwrap().text;
    let expected = r#"for(let i = 0; i < testVariable >> 1; i++)callCount++;"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn jsx_spread_works() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const A = () => {
  return <div>{...[]}</div>;
};"#;
    let parsed_source = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    assert!(parsed_source.transpile(&Default::default()).is_ok());
  }

  #[test]
  fn diagnostic_octal_and_leading_zero_num_literals() {
    assert_eq!(get_diagnostic("077"), concat!(
      "Legacy octal literals are not available when targeting ECMAScript 5 and higher ",
      "at https://deno.land/x/mod.ts:1:1\n\n",
      "  077\n",
      "  ~~~\n\n",
      "Legacy octal escape is not permitted in strict mode at https://deno.land/x/mod.ts:1:1\n\n",
      "  077\n",
      "  ~~~",
    ));
    assert_eq!(get_diagnostic("099"), concat!(
      "Legacy decimal escape is not permitted in strict mode at https://deno.land/x/mod.ts:1:1\n\n",
      "  099\n",
      "  ~~~",
    ));
  }

  #[test]
  fn diagnostic_missing_brace() {
    assert_eq!(
      get_diagnostic("function test() {"),
      concat!(
        "Expected '}', got '<eof>' at https://deno.land/x/mod.ts:1:17\n\n",
        "  function test() {\n",
        "                  ~",
      ),
    );
  }

  #[test]
  fn diagnostic_nullish_coalescing_with_logical_op() {
    assert_eq!(
      get_diagnostic("null || undefined ?? 'foo';"),
      concat!(
        "Nullish coalescing operator(??) requires parens when mixing with logical operators at https://deno.land/x/mod.ts:1:1\n\n",
        "  null || undefined ?? 'foo';\n",
        "  ~~~~~~~~~~~~~~~~~",
      )
    );
    assert_eq!(
      get_diagnostic("null && undefined ?? 'foo';"),
      concat!(
        "Nullish coalescing operator(??) requires parens when mixing with logical operators at https://deno.land/x/mod.ts:1:1\n\n",
        "  null && undefined ?? 'foo';\n",
        "  ~~~~~~~~~~~~~~~~~",
      ),
    );
  }

  fn get_diagnostic(source: &str) -> String {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let parsed_source = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    parsed_source
      .transpile(&Default::default())
      .err()
      .unwrap()
      .to_string()
  }

  #[test]
  fn source_map_properly_encoded() {
    // Ref https://github.com/denoland/deno/issues/10936
    // Ref https://github.com/swc-project/swc/issues/3288#issuecomment-1117252904

    let p = parse_module(ParseParams {
      specifier: "file:///Users/ib/dev/deno/foo.ts".to_string(),
      text_info: SourceTextInfo::from_string(
        r#"export default function () {
    return "📣❓";
}"#
          .to_string(),
      ),
      media_type: MediaType::TypeScript,
      capture_tokens: true,
      scope_analysis: false,
      maybe_syntax: None,
    })
    .unwrap();

    let transpiled = p.transpile(&Default::default()).unwrap();
    let lines: Vec<&str> = transpiled.text.split('\n').collect();
    let last_line = lines.last().unwrap();
    let input = last_line
      .trim_start_matches("//# sourceMappingURL=data:application/json;base64,");
    base64::decode(input).unwrap();
  }
}
