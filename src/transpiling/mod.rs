// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::rc::Rc;
use std::sync::Arc;

use anyhow::Result;
use swc_ecma_visit::as_folder;
use thiserror::Error;

use crate::emit;
use crate::swc::ast::Program;
use crate::swc::common::chain;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::errors::Diagnostic as SwcDiagnostic;
use crate::swc::common::Globals;
use crate::swc::common::Mark;
use crate::swc::parser::error::SyntaxError;
use crate::swc::transforms::fixer;
use crate::swc::transforms::helpers;
use crate::swc::transforms::hygiene;
use crate::swc::transforms::pass::Optional;
use crate::swc::transforms::proposal;
use crate::swc::transforms::react;
use crate::swc::transforms::resolver;
use crate::swc::transforms::typescript;
use crate::swc::visit::FoldWith;
use crate::EmitError;
use crate::EmitOptions;
use crate::EmittedSource;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::ParseDiagnosticsError;
use crate::ParsedSource;
use crate::SourceMap;

use std::cell::RefCell;

mod jsx_precompile;
mod transforms;

#[derive(Debug, Error)]
pub enum TranspileError {
  #[error("Can't use TranspileOptions::use_decorators_proposal and TranspileOptions::use_ts_decorators together.")]
  DecoratorOptionsConflict,
  /// Parse errors that prevent transpiling.
  #[error(transparent)]
  ParseErrors(#[from] ParseDiagnosticsError),
  #[error(transparent)]
  FoldProgram(#[from] FoldProgramError),
  #[error("{0}")]
  EmitDiagnostic(String),
  #[error(transparent)]
  Emit(#[from] EmitError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportsNotUsedAsValues {
  Remove,
  Preserve,
  Error,
}

/// Options which can be adjusted when transpiling a module.
///
/// This implements `Hash` so the CLI can use it to bust the emit cache.
#[derive(Debug, Clone, Hash)]
pub struct TranspileOptions {
  /// TypeScript experimental decorators.
  pub use_ts_decorators: bool,

  /// TC39 Decorators Proposal - https://github.com/tc39/proposal-decorators
  pub use_decorators_proposal: bool,

  /// When emitting a legacy decorator, also emit experimental decorator meta
  /// data.  Defaults to `false`.
  pub emit_metadata: bool,
  /// What to do with import statements that only import types i.e. whether to
  /// remove them (`Remove`), keep them as side-effect imports (`Preserve`)
  /// or error (`Error`). Defaults to `Remove`.
  pub imports_not_used_as_values: ImportsNotUsedAsValues,
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
  /// Should JSX be transformed. Defaults to `true`.
  pub transform_jsx: bool,
  /// Should JSX be precompiled into static strings that need to be concatenated
  /// with dynamic content. Defaults to `false`, mutually exclusive with
  /// `transform_jsx`.
  pub precompile_jsx: bool,
  /// Should import declarations be transformed to variable declarations using
  /// a dynamic import. This is useful for import & export declaration support
  /// in script contexts such as the Deno REPL.  Defaults to `false`.
  pub var_decl_imports: bool,
}

impl Default for TranspileOptions {
  fn default() -> Self {
    TranspileOptions {
      use_ts_decorators: false,
      use_decorators_proposal: false,
      emit_metadata: false,
      imports_not_used_as_values: ImportsNotUsedAsValues::Remove,
      jsx_automatic: false,
      jsx_development: false,
      jsx_factory: "React.createElement".into(),
      jsx_fragment_factory: "React.Fragment".into(),
      jsx_import_source: None,
      transform_jsx: true,
      precompile_jsx: false,
      var_decl_imports: false,
    }
  }
}

impl TranspileOptions {
  fn as_tsx_config(&self) -> typescript::TsxConfig {
    typescript::TsxConfig {
      pragma: Some(self.jsx_factory.clone()),
      pragma_frag: Some(self.jsx_fragment_factory.clone()),
    }
  }

  fn as_typescript_config(&self) -> typescript::Config {
    typescript::Config {
      verbatim_module_syntax: false,
      import_not_used_as_values: match self.imports_not_used_as_values {
        ImportsNotUsedAsValues::Remove => {
          typescript::ImportsNotUsedAsValues::Remove
        }
        ImportsNotUsedAsValues::Preserve => {
          typescript::ImportsNotUsedAsValues::Preserve
        }
        // `Error` only affects the type-checking stage. Fall back to `Remove` here.
        ImportsNotUsedAsValues::Error => {
          typescript::ImportsNotUsedAsValues::Remove
        }
      },
      // no need for this to be false because we treat all files as modules
      no_empty_export: true,
      // we don't suport this, so leave it as-is so it errors in v8
      import_export_assign_config:
        typescript::TsImportExportAssignConfig::Preserve,
      // Do not opt into swc's optimization to inline enum member values
      // in the same module as it might cause bugs in certain code.
      ts_enum_is_mutable: true,
    }
  }
}

impl ParsedSource {
  /// Transform a TypeScript file into a JavaScript file.
  ///
  /// Note: This will clone the program if it's shared, which
  /// might be expensive.
  pub fn transpile(
    &self,
    transpile_options: &TranspileOptions,
    emit_options: &EmitOptions,
  ) -> Result<EmittedSource, TranspileError> {
    let program = (*self.program()).clone();
    transpile(
      self.specifier().clone(),
      self.text_info().text_str().to_string(),
      program,
      // we need the comments to be mutable, so make it single threaded
      self.comments().as_single_threaded(),
      transpile_options,
      emit_options,
      self.diagnostics(),
    )
  }

  /// Transform a TypeScript file into a JavaScript file consuming
  /// the internals of the `ParsedSource`. Returns an `Err(ParsedSource)`
  /// when the `ParsedSource` is shared.
  pub fn transpile_owned(
    self,
    transpile_options: &TranspileOptions,
    emit_options: &EmitOptions,
  ) -> Result<Result<EmittedSource, TranspileError>, ParsedSource> {
    let inner = match Arc::try_unwrap(self.inner) {
      Ok(inner) => inner,
      Err(inner) => return Err(ParsedSource { inner }),
    };
    let program = match Arc::try_unwrap(inner.program) {
      Ok(program) => program,
      Err(program) => {
        return Err(ParsedSource {
          inner: Arc::new(crate::ParsedSourceInner {
            specifier: inner.specifier,
            media_type: inner.media_type,
            text_info: inner.text_info,
            comments: inner.comments,
            program,
            tokens: inner.tokens,
            syntax_contexts: inner.syntax_contexts,
            diagnostics: inner.diagnostics,
          }),
        })
      }
    };
    Ok(transpile(
      inner.specifier,
      inner.text_info.text_str().to_string(),
      program,
      // we need the comments to be mutable, so make it single threaded
      inner.comments.into_single_threaded(),
      transpile_options,
      emit_options,
      &inner.diagnostics,
    ))
  }

  /// Attempts to transpile owned, then falls back to cloning the program.
  pub fn transpile_owned_with_fallback(
    self,
    transpile_options: &TranspileOptions,
    emit_options: &EmitOptions,
  ) -> Result<EmittedSource, TranspileError> {
    match self.transpile_owned(transpile_options, emit_options) {
      Ok(result) => result,
      Err(parsed_source) => {
        // fallback
        parsed_source.transpile(transpile_options, emit_options)
      }
    }
  }
}

fn transpile(
  specifier: ModuleSpecifier,
  source: String,
  program: Program,
  comments: SingleThreadedComments,
  transpile_options: &TranspileOptions,
  emit_options: &EmitOptions,
  diagnostics: &[ParseDiagnostic],
) -> Result<EmittedSource, TranspileError> {
  if transpile_options.use_decorators_proposal
    && transpile_options.use_ts_decorators
  {
    return Err(TranspileError::DecoratorOptionsConflict);
  }

  let source_map = SourceMap::single(specifier, source);

  let globals = Globals::new();
  let program = crate::swc::common::GLOBALS.set(&globals, || {
    let top_level_mark = Mark::fresh(Mark::root());
    fold_program(
      program,
      transpile_options,
      &source_map,
      &comments,
      top_level_mark,
      diagnostics,
    )
  })?;

  Ok(emit(&program, &comments, &source_map, emit_options)?)
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

#[derive(Debug, Error)]
pub enum FoldProgramError {
  #[error(transparent)]
  ParseDiagnostics(#[from] ParseDiagnosticsError),
  #[error(transparent)]
  Swc(#[from] SwcFoldDiagnosticsError),
}

/// Low level function for transpiling a program.
pub fn fold_program(
  program: Program,
  options: &TranspileOptions,
  source_map: &SourceMap,
  comments: &SingleThreadedComments,
  top_level_mark: Mark,
  diagnostics: &[ParseDiagnostic],
) -> Result<Program, FoldProgramError> {
  ensure_no_fatal_diagnostics(diagnostics)?;

  let unresolved_mark = Mark::new();
  let mut passes = chain!(
    Optional::new(transforms::StripExportsFolder, options.var_decl_imports),
    resolver(unresolved_mark, top_level_mark, true),
    Optional::new(
      proposal::decorators::decorators(proposal::decorators::Config {
        legacy: true,
        emit_metadata: options.emit_metadata,

        use_define_for_class_fields: true,
      }),
      options.use_ts_decorators,
    ),
    Optional::new(
      proposal::decorator_2022_03::decorator_2022_03(),
      options.use_decorators_proposal,
    ),
    proposal::explicit_resource_management::explicit_resource_management(),
    helpers::inject_helpers(top_level_mark),
    // transform imports to var decls before doing the typescript pass
    // so that swc doesn't do any optimizations on the import declarations
    Optional::new(
      transforms::ImportDeclsToVarDeclsFolder,
      options.var_decl_imports
    ),
    Optional::new(
      typescript::typescript(options.as_typescript_config(), top_level_mark),
      !options.transform_jsx
    ),
    Optional::new(
      typescript::tsx(
        source_map.inner().clone(),
        options.as_typescript_config(),
        options.as_tsx_config(),
        comments,
        top_level_mark
      ),
      options.transform_jsx
    ),
    Optional::new(
      as_folder(jsx_precompile::JsxPrecompile::new(
        options.jsx_import_source.clone().unwrap_or_default(),
      )),
      options.jsx_import_source.is_some()
        && !options.transform_jsx
        && options.precompile_jsx
    ),
    Optional::new(
      react::react(
        source_map.inner().clone(),
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
    // if using var decl imports, do another pass in order to transform the
    // automatically inserted jsx runtime import to a var decl
    Optional::new(
      transforms::ImportDeclsToVarDeclsFolder,
      options.var_decl_imports && options.transform_jsx
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

  let mut diagnostics = diagnostics_cell.borrow_mut();
  let diagnostics = std::mem::take(&mut *diagnostics);
  ensure_no_fatal_swc_diagnostics(source_map, diagnostics.into_iter())?;
  Ok(result)
}

#[derive(Debug)]
pub struct SwcFoldDiagnosticsError(Vec<String>);

impl std::error::Error for SwcFoldDiagnosticsError {}

impl std::fmt::Display for SwcFoldDiagnosticsError {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    for (i, diagnostic) in self.0.iter().enumerate() {
      if i > 0 {
        write!(f, "\n\n")?;
      }

      write!(f, "{}", diagnostic)?
    }

    Ok(())
  }
}

fn ensure_no_fatal_swc_diagnostics(
  source_map: &SourceMap,
  diagnostics: impl Iterator<Item = SwcDiagnostic>,
) -> Result<(), SwcFoldDiagnosticsError> {
  let fatal_diagnostics = diagnostics
    .filter(is_fatal_swc_diagnostic)
    .collect::<Vec<_>>();
  if !fatal_diagnostics.is_empty() {
    Err(SwcFoldDiagnosticsError(
      fatal_diagnostics
        .iter()
        .map(|d| format_swc_diagnostic(source_map, d))
        .collect::<Vec<_>>(),
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
    let source_map = source_map.inner();
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
  diagnostics: &[ParseDiagnostic],
) -> Result<(), ParseDiagnosticsError> {
  let fatal_diagnostics = diagnostics
    .iter()
    .filter(|d| is_fatal_syntax_error(&d.kind))
    .map(ToOwned::to_owned)
    .collect::<Vec<_>>();
  if !fatal_diagnostics.is_empty() {
    Err(ParseDiagnosticsError(fatal_diagnostics))
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
        // init required for using
        SyntaxError::InitRequiredForUsingDecl |
        // missing a token
        SyntaxError::Expected(_, _)
  )
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::parse_module;
  use crate::MediaType;
  use crate::ModuleSpecifier;
  use crate::ParseParams;
  use crate::SourceMapOption;
  use crate::SourceTextInfo;

  use base64::Engine;
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

const enum E {
  A,
  B,
}

console.log(E.A);

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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap();
    let expected_text = r#"var D;
(function(D) {
  D[D["A"] = 0] = "A";
  D[D["B"] = 1] = "B";
})(D || (D = {}));
var E;
console.log(0);
var N;
(function(N) {
  let D;
  (function(D) {
    D["A"] = "value";
  })(D = N.D || (N.D = {}));
  N.Value = 5;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap();
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
  if (Object(value) !== value) {
    throw new TypeError("using declarations can only be used with objects, functions, null, or undefined.");
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap();
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap();
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap()
      .text;
    let expected = r#"import { h, Fragment } from "https://deno.land/x/mod.ts";
function App() {
  return h("div", null, h(Fragment, null));
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = module
      .transpile(
        &TranspileOptions::default(),
        &EmitOptions {
          keep_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx_automatic: true,
      jsx_import_source: Some("jsx_lib".to_string()),
      ..Default::default()
    };
    let code = module
      .transpile(&transpile_options, &EmitOptions::default())
      .unwrap()
      .text;
    let expected = r#"import { jsx as _jsx, Fragment as _Fragment } from "jsx_lib/jsx-runtime";
function App() {
  return _jsx("div", {
    children: _jsx(_Fragment, {})
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx_automatic: true,
      jsx_import_source: Some("jsx_lib".to_string()),
      jsx_development: true,
      ..Default::default()
    };
    let code = module
      .transpile(&transpile_options, &EmitOptions::default())
      .unwrap()
      .text;
    let expected = r#"import { jsxDEV as _jsxDEV, Fragment as _Fragment } from "jsx_lib/jsx-dev-runtime";
function App() {
  return _jsxDEV("div", {
    children: _jsxDEV(_Fragment, {}, void 0, false)
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
import * as example from "example";

function App() {
  return (
    <div><></></div>
  );
}"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      var_decl_imports: true,
      ..Default::default()
    };
    let code = module
      .transpile(&transpile_options, &EmitOptions::default())
      .unwrap()
      .text;
    let expected = r#"const { "jsx": _jsx, "Fragment": _Fragment } = await import("jsx_lib/jsx-runtime");
const example = await import("example");
function App() {
  return _jsx("div", {
    children: _jsx(_Fragment, {})
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module
      .transpile(
        &TranspileOptions {
          use_ts_decorators: true,
          ..Default::default()
        },
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
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
  fn test_transpile_decorators_proposal() {
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
        return descriptor;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module
      .transpile(
        &TranspileOptions {
          use_decorators_proposal: true,
          ..Default::default()
        },
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    let expected =
      include_str!("./testdata/tc39_decorator_proposal_output.txt");
    assert_eq!(&code[0..expected.len()], expected);
  }

  #[test]
  fn test_transpile_decorators_both() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = "";
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    module
      .transpile(
        &TranspileOptions {
          use_decorators_proposal: true,
          use_ts_decorators: true,
          ..Default::default()
        },
        &EmitOptions::default(),
      )
      .unwrap_err();
  }

  #[test]
  fn test_transpile_no_decorators() {
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
        return descriptor;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module
      .transpile(&TranspileOptions::default(), &EmitOptions::default())
      .unwrap()
      .text;
    let expected = r#"function enumerable(value) {
  return function(_target, _propertyKey, descriptor) {
    descriptor.enumerable = value;
    return descriptor;
  };
}
export class A {
  @enumerable(false)
  a() {
    Test.value;
  }
}
"#;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      transform_jsx: true,
      ..Default::default()
    };
    let code = module
      .transpile(&transpile_options, &EmitOptions::default())
      .unwrap()
      .text;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = module
      .transpile(&Default::default(), &EmitOptions::default())
      .unwrap()
      .text;
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
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    assert!(parsed_source
      .transpile(&Default::default(), &EmitOptions::default())
      .is_ok());
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

  #[test]
  fn diagnostic_missing_init_in_using() {
    assert_eq!(get_diagnostic("using test"), concat!(
      "Using declaration requires initializer at https://deno.land/x/mod.ts:1:1\n\n",
      "  using test\n",
      "  ~~~~~~~~~~",
    ));
  }

  fn get_diagnostic(source: &str) -> String {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let parsed_source = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    parsed_source
      .transpile(&Default::default(), &EmitOptions::default())
      .err()
      .unwrap()
      .to_string()
  }

  #[test]
  fn source_map_properly_encoded() {
    // Ref https://github.com/denoland/deno/issues/10936
    // Ref https://github.com/swc-project/swc/issues/3288#issuecomment-1117252904

    let p = parse_module(ParseParams {
      specifier: ModuleSpecifier::parse("file:///Users/ib/dev/deno/foo.ts")
        .unwrap(),
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

    let transpiled = p
      .transpile(&Default::default(), &EmitOptions::default())
      .unwrap();
    let lines: Vec<&str> = transpiled.text.split('\n').collect();
    let last_line = lines.last().unwrap();
    let input = last_line
      .trim_start_matches("//# sourceMappingURL=data:application/json;base64,");
    base64::prelude::BASE64_STANDARD.decode(input).unwrap();
  }

  #[test]
  fn test_precompile_jsx() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source =
      r#"const a = <Foo><span>hello</span>foo<Bar><p>asdf</p></Bar></Foo>;"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      transform_jsx: false,
      precompile_jsx: true,
      jsx_import_source: Some("react".to_string()),
      ..Default::default()
    };
    let code = module
      .transpile(&transpile_options, &EmitOptions::default())
      .unwrap()
      .text;
    let expected1 = r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate } from "react/jsx-runtime";
const $$_tpl_2 = [
  "<p>asdf</p>"
];
const $$_tpl_1 = [
  "<span>hello</span>foo",
  ""
];
const a = _jsx(Foo, {
  children: _jsxTemplate($$_tpl_1, _jsx(Bar, {
    children: _jsxTemplate($$_tpl_2)
  }))
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJza"#;
    assert_eq!(&code[0..expected1.len()], expected1);
  }

  #[test]
  fn test_inline_source_map_newline() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Inline,
      ..Default::default()
    };
    let emit_result = module
      .transpile(&TranspileOptions::default(), &emit_options)
      .unwrap();
    let expected1 = r#"{
  const foo = "bar";
}
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJza"#;
    assert_eq!(&emit_result.text[0..expected1.len()], expected1);
    assert_eq!(emit_result.source_map, None);
  }

  #[test]
  fn test_source_map() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Separate,
      ..Default::default()
    };
    let emit_result = module
      .transpile(&TranspileOptions::default(), &emit_options)
      .unwrap();
    assert_eq!(
      &emit_result.text,
      r#"{
  const foo = "bar";
}"#
    );
    assert_eq!(
      emit_result.source_map.as_deref(),
      Some(
        r#"{"version":3,"sources":["https://deno.land/x/mod.tsx"],"sourcesContent":["{ const foo = \"bar\"; };"],"names":[],"mappings":"AAAA;EAAE,MAAM,MAAM;AAAO"}"#
      )
    );
  }

  #[test]
  fn test_no_source_map() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::None,
      ..Default::default()
    };
    let emit_result = module
      .transpile(&TranspileOptions::default(), &emit_options)
      .unwrap();
    assert_eq!(
      &emit_result.text,
      r#"{
  const foo = "bar";
}"#
    );
    assert_eq!(emit_result.source_map, None);
  }

  #[test]
  fn test_transpile_owned_when_owned() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const foo: string = "bar";"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_result = module
      .transpile_owned(
        &TranspileOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap()
      .unwrap();
    assert_eq!(&emit_result.text, "const foo = \"bar\";\n");
  }

  #[test]
  fn test_transpile_owned_when_cloned() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const foo: string = "bar";"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();

    // won't transpile since the module is cloned
    let borrowed_module = module.clone();
    let result =
      module.transpile_owned(&Default::default(), &Default::default());
    let module = result.err().unwrap();
    drop(borrowed_module);

    // won't transpile since the program is cloned
    let borrowed_program = module.program().clone();
    let result =
      module.transpile_owned(&Default::default(), &Default::default());
    let module = result.err().unwrap();
    drop(borrowed_program);

    // now it will work
    let emit_result = module
      .transpile_owned(
        &TranspileOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap()
      .unwrap();
    assert_eq!(&emit_result.text, "const foo = \"bar\";\n");
  }

  #[test]
  fn test_transpile_owned_with_fallback() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const foo: string = "bar";"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();

    // even though it's borrowed, it will transpile
    let borrowed_module = module.clone();
    let emit_result = module
      .transpile_owned_with_fallback(
        &TranspileOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap();
    assert_eq!(&emit_result.text, "const foo = \"bar\";\n");

    // now it's owned, should still work
    let emit_result = borrowed_module
      .transpile_owned_with_fallback(
        &TranspileOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap();
    assert_eq!(&emit_result.text, "const foo = \"bar\";\n");
  }
  
  #[test]
  fn should_not_panic_with_scope_analysis() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
const inspect: () => void = eval();

export function defaultFormatter(record: Record): string {
  for (let i = 0; i < 10; i++) {
    inspect(record);
  }
}

export function formatter(record: Record) {
}
"#;
    let module = parse_module(ParseParams {
      specifier,
      text_info: SourceTextInfo::from_string(source.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();

    let emit_result = module
      .transpile(
        &TranspileOptions::default(),
        &EmitOptions::default(),
      );
    assert!(emit_result.is_ok());
  }
}
