// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

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
use crate::swc::transforms::proposals;
use crate::swc::transforms::react;
use crate::swc::transforms::resolver_with_mark;
use crate::swc::transforms::typescript;
use crate::swc::visit::FoldWith;
use crate::Diagnostic;
use crate::DiagnosticsError;
use crate::ModuleSpecifier;
use crate::ParsedSource;

use std::cell::RefCell;

mod transforms;

#[derive(Debug, Clone)]
pub enum ImportsNotUsedAsValues {
  Remove,
  Preserve,
  Error,
}

/// Options which can be adjusted when transpiling a module.
#[derive(Debug, Clone)]
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
  /// Should JSX be transformed or preserved.  Defaults to `true`.
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
      use_define_for_class_fields: true,
      // TODO(bartlomieju): this could be changed to `false` to provide `export {}`
      // in Typescript files without manual changes
      no_empty_export: true,
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
    source_map.new_source_file(file_name, self.source().text().to_string());
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
        let writer = Box::new(JsWriter::new(
          source_map.clone(),
          "\n",
          &mut buf,
          Some(&mut src_map_buf),
        ));
        let config = crate::swc::codegen::Config { minify: false };
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
          .build_source_map_with_config(
            &mut src_map_buf,
            None,
            source_map_config,
          )
          .to_writer(&mut buf)?;

        if options.inline_source_map {
          src.push_str("//# sourceMappingURL=data:application/json;base64,");
          let encoded_map = base64::encode(buf);
          src.push_str(&encoded_map);
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

  let jsx_pass = react::react(
    source_map.clone(),
    Some(comments),
    react::Options {
      pragma: options.jsx_factory.clone(),
      pragma_frag: options.jsx_fragment_factory.clone(),
      // this will use `Object.assign()` instead of the `_extends` helper
      // when spreading props.
      use_builtins: true,
      runtime: if options.jsx_automatic {
        Some(react::Runtime::Automatic)
      } else {
        None
      },
      development: options.jsx_development,
      import_source: options.jsx_import_source.clone().unwrap_or_default(),
      ..Default::default()
    },
    top_level_mark,
  );
  let mut passes = chain!(
    Optional::new(
      transforms::ImportDeclsToVarDeclsFolder,
      options.var_decl_imports
    ),
    Optional::new(transforms::StripExportsFolder, options.var_decl_imports),
    proposals::decorators::decorators(proposals::decorators::Config {
      legacy: true,
      emit_metadata: options.emit_metadata
    }),
    helpers::inject_helpers(),
    resolver_with_mark(top_level_mark),
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
    Optional::new(jsx_pass, options.transform_jsx),
    fixer(Some(comments)),
    hygiene(),
  );

  let emitter = DiagnosticCollector::default();
  let diagnostics_cell = emitter.diagnostics_cell.clone();
  let handler = emitter.into_handler();
  let result = crate::swc::utils::HANDLER.set(&handler, || {
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
      file_name.to_string(),
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
        // expected expression
        SyntaxError::TS1109 |
        // unterminated string literal
        SyntaxError::UnterminatedStrLit
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
      source: SourceTextInfo::from_string(source.to_string()),
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
(function(N1) {
    let D;
    (function(D) {
        D["A"] = "value";
    })(D = N1.D || (N1.D = {}));
    var Value = N1.Value = 5;
})(N || (N = {}));
export class A {
    d;
    b;
    c = 1;
    e;
    constructor(d = D.A){
        this.d = d;
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
      source: SourceTextInfo::from_string(source.to_string()),
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
      source: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    let expected = r#"/** @jsx h */ /** @jsxFrag Fragment */ import { h, Fragment } from "https://deno.land/x/mod.ts";
function App() {
    return(/*#__PURE__*/ h("div", null, /*#__PURE__*/ h(Fragment, null)));
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
      source: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    let expected = r#"import { jsx as _jsx, Fragment as _Fragment } from "jsx_lib/jsx-runtime";
/** @jsxImportSource jsx_lib */ function App() {
    return(/*#__PURE__*/ _jsx("div", {
        children: /*#__PURE__*/ _jsx(_Fragment, {})
    }));
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
      source: SourceTextInfo::from_string(source.to_string()),
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
    return(/*#__PURE__*/ _jsx("div", {
        children: /*#__PURE__*/ _jsx(_Fragment, {})
    }));
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
      source: SourceTextInfo::from_string(source.to_string()),
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
    return(/*#__PURE__*/ _jsxDEV("div", {
        children: /*#__PURE__*/ _jsxDEV(_Fragment, {}, void 0, false)
    }, void 0, false, {
        fileName: "https://deno.land/x/mod.tsx",
        lineNumber: 3,
        columnNumber: 5
    }, this));
}
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
      source: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module.transpile(&EmitOptions::default()).unwrap().text;
    assert!(code.contains("_applyDecoratedDescriptor("));
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
      source: SourceTextInfo::from_string(source.to_string()),
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
  fn diagnostic_jsx_spread_instead_of_panic() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const A = () => {
  return <div>{...[]}</div>;
};"#;
    let parsed_source = parse_module(ParseParams {
      specifier: specifier.as_str().to_string(),
      source: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let err = parsed_source.transpile(&Default::default()).err().unwrap();

    assert_eq!(err.to_string(), "Spread children are not supported in React. at https://deno.land/x/mod.ts:2:15");
  }
}
