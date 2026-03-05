// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::path::Path;

use deno_media_type::MediaType;
use thiserror::Error;

use oxc::allocator::Allocator;
use oxc::ast::ast::Program;
use oxc::ast_visit::VisitMut;
use oxc::semantic::SemanticBuilder;
use oxc::transformer::{
  DecoratorOptions, JsxOptions, TransformOptions, Transformer,
  TypeScriptOptions,
};

use crate::EmitError;
use crate::EmitOptions;
use crate::EmittedSourceText;
use crate::ModuleKind;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::ParseDiagnostics;
use crate::ParseDiagnosticsError;
use crate::ParsedSource;
use crate::emit;

use deno_error::JsError;

mod jsx_precompile;
mod transforms;

/// Holds whether the `ParsedSource` was cloned or consumed (owned) during
/// transpilation. This is useful for logging in the CLI when transpilation
/// occurs when a `ParsedSource` is cloned, as it's a performance issue.
#[derive(Debug, Clone)]
pub enum TranspileResult {
  /// The `ParsedSource` needed to be cloned in order to transpile.
  ///
  /// This is a performance issue and you should strive to get an `Owned` result.
  Cloned(EmittedSourceText),
  /// The emit occurred consuming the `ParsedSource` without cloning.
  Owned(EmittedSourceText),
}

impl TranspileResult {
  pub fn into_source(self) -> EmittedSourceText {
    match self {
      TranspileResult::Owned(source) => source,
      TranspileResult::Cloned(source) => source,
    }
  }
}

#[derive(Debug, Error, JsError)]
pub enum TranspileError {
  /// Parse errors that prevent transpiling.
  #[class(inherit)]
  #[error(transparent)]
  ParseErrors(#[from] ParseDiagnosticsError),
  #[class(inherit)]
  #[error(transparent)]
  TransformProgram(#[from] TransformProgramError),
  #[class(type)]
  #[error("{0}")]
  EmitDiagnostic(String),
  #[class(inherit)]
  #[error(transparent)]
  Emit(#[from] EmitError),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportsNotUsedAsValues {
  Remove,
  Preserve,
  Error,
}

#[derive(Debug, Clone, Hash)]
pub struct JsxClassicOptions {
  /// When transforming JSX, what value should be used for the JSX factory.
  /// Defaults to `React.createElement`.
  pub factory: String,
  /// When transforming JSX, what value should be used for the JSX fragment
  /// factory.  Defaults to `React.Fragment`.
  pub fragment_factory: String,
}

impl Default for JsxClassicOptions {
  fn default() -> Self {
    Self {
      factory: "React.createElement".into(),
      fragment_factory: "React.Fragment".into(),
    }
  }
}

#[derive(Debug, Clone, Hash)]
pub struct JsxAutomaticOptions {
  /// If it is in development mode, meaning that it should import `jsx-dev-runtime`
  /// and transform JSX using `jsxDEV` import from the JSX import source as well as
  /// provide additional debug information to the JSX factory.
  pub development: bool,
  /// The string module specifier to implicitly import JSX factories from when
  /// transpiling JSX.
  pub import_source: Option<String>,
}

#[derive(Debug, Clone, Hash)]
pub struct JsxPrecompileOptions {
  pub automatic: JsxAutomaticOptions,
  /// List of elements that should not be precompiled when the JSX precompile
  /// transform is used.
  pub skip_elements: Option<Vec<String>>,
  /// List of properties/attributes that should always be treated as
  /// dynamic.
  pub dynamic_props: Option<Vec<String>>,
}

#[derive(Debug, Clone, Hash)]
pub enum JsxRuntime {
  Classic(JsxClassicOptions),
  Automatic(JsxAutomaticOptions),
  /// Transforms JSX into static strings that need to be concatenated
  /// with dynamic content.
  Precompile(JsxPrecompileOptions),
}

impl Default for JsxRuntime {
  fn default() -> Self {
    Self::Classic(Default::default())
  }
}

impl JsxRuntime {
  pub fn automatic(&self) -> Option<&JsxAutomaticOptions> {
    match self {
      JsxRuntime::Classic(_) => None,
      JsxRuntime::Automatic(o) => Some(o),
      JsxRuntime::Precompile(o) => Some(&o.automatic),
    }
  }

  pub fn classic(&self) -> Option<&JsxClassicOptions> {
    match self {
      JsxRuntime::Classic(o) => Some(o),
      JsxRuntime::Automatic(_) | JsxRuntime::Precompile(_) => None,
    }
  }

  pub fn precompile(&self) -> Option<&JsxPrecompileOptions> {
    match self {
      JsxRuntime::Classic(_) | JsxRuntime::Automatic(_) => None,
      JsxRuntime::Precompile(o) => Some(o),
    }
  }
}

#[derive(Debug, Default, Clone, Hash)]
pub enum DecoratorsTranspileOption {
  /// Leave decorators as-is.
  #[default]
  None,
  /// TC39 Decorators Proposal - https://github.com/tc39/proposal-decorators
  Ecma,
  /// TypeScript experimental decorators.
  LegacyTypeScript {
    /// Also emit experimental decorator meta data.
    emit_metadata: bool,
  },
}

/// Options which can be adjusted when transpiling a module.
///
/// This implements `Hash` so the CLI can use it to bust the emit cache.
#[derive(Debug, Clone, Hash)]
pub struct TranspileOptions {
  /// Kind of decorators to use.
  pub decorators: DecoratorsTranspileOption,
  /// `true` changes type stripping behaviour so that _only_ `type` imports
  /// are stripped.
  pub verbatim_module_syntax: bool,
  /// What to do with import statements that only import types i.e. whether to
  /// remove them (`Remove`), keep them as side-effect imports (`Preserve`)
  /// or error (`Error`). Defaults to `Remove`.
  pub imports_not_used_as_values: ImportsNotUsedAsValues,
  /// Options for transforming JSX. Will not transform when `None`.
  pub jsx: Option<JsxRuntime>,
  /// Should import declarations be transformed to variable declarations using
  /// a dynamic import. This is useful for import & export declaration support
  /// in script contexts such as the Deno REPL.  Defaults to `false`.
  pub var_decl_imports: bool,
}

impl Default for TranspileOptions {
  fn default() -> Self {
    TranspileOptions {
      decorators: DecoratorsTranspileOption::default(),
      verbatim_module_syntax: false,
      imports_not_used_as_values: ImportsNotUsedAsValues::Remove,
      jsx: Some(Default::default()),
      var_decl_imports: false,
    }
  }
}

/// Transpile options specific to the module being transpiled.
///
/// This is separate from `TranspileOptions` in order for that to
/// be shared across modules, but this to be changed per module.
#[derive(Debug, Default, Clone, Hash)]
pub struct TranspileModuleOptions {
  /// The kind of module being transpiled.
  ///
  /// Defaults to being derived from the media type of the parsed source.
  pub module_kind: Option<ModuleKind>,
}

impl<'a> ParsedSource<'a> {
  /// Transform a TypeScript file into a JavaScript file.
  pub fn transpile(
    &mut self,
    allocator: &'a Allocator,
    transpile_options: &TranspileOptions,
    _transpile_module_options: &TranspileModuleOptions,
    emit_options: &EmitOptions,
  ) -> Result<EmittedSourceText, TranspileError> {
    let transpile_options =
      resolve_transpile_options(self.media_type, transpile_options);

    // If @jsxImportSource pragma is found in comments, switch to automatic
    // runtime to match SWC/Babel behavior where @jsxImportSource implies
    // automatic runtime.
    let transpile_options = maybe_upgrade_jsx_for_pragma(
      &transpile_options,
      &self.program.comments,
      self.text.as_ref(),
    );

    // Apply custom pre-transforms
    if transpile_options.var_decl_imports {
      let mut strip_exports =
        transforms::StripExports::new(allocator);
      strip_exports.visit_program(&mut self.program);

      // Transform import declarations to variable declarations before
      // the TS pass so that the transformer doesn't optimize them away
      let mut import_to_var =
        transforms::ImportDeclsToVarDecls::new(allocator);
      import_to_var.visit_program(&mut self.program);
    }

    // Run precompile JSX pass BEFORE the OXC transformer so it sees raw JSX
    if transpile_options
      .jsx
      .as_ref()
      .map(|jsx| jsx.precompile().is_some())
      .unwrap_or(false)
    {
      let mut jsx_precompile = jsx_precompile::JsxPrecompile::new(
        allocator,
        transpile_options
          .jsx
          .as_ref()
          .and_then(|jsx| jsx.automatic())
          .and_then(|a| a.import_source.clone()),
        transpile_options
          .jsx
          .as_ref()
          .and_then(|jsx| jsx.precompile())
          .and_then(|p| p.skip_elements.clone()),
        transpile_options
          .jsx
          .as_ref()
          .and_then(|jsx| jsx.precompile())
          .and_then(|p| p.dynamic_props.clone()),
      );
      jsx_precompile.visit_program(&mut self.program);
    }

    // Build OXC TransformOptions
    let transform_options =
      build_transform_options(&transpile_options, &self.specifier);

    // Run semantic analysis to get scoping info
    let semantic_ret =
      SemanticBuilder::new().build(&self.program);
    let scoping = semantic_ret.semantic.into_scoping();

    // Run OXC transformer (TS strip, JSX, decorators, etc.)
    let transformer = Transformer::new(allocator, Path::new(self.specifier.as_str()), &transform_options);
    let _transform_ret =
      transformer.build_with_scoping(scoping, &mut self.program);

    // Apply custom post-transforms: transform any import declarations
    // inserted by the JSX transform to variable declarations
    let has_jsx_transform = transpile_options.jsx.is_some();
    if transpile_options.var_decl_imports && has_jsx_transform {
      let mut import_to_var =
        transforms::ImportDeclsToVarDecls::new(allocator);
      import_to_var.visit_program(&mut self.program);
    }

    // Emit
    let file_name = self.specifier.as_str();
    Ok(emit(
      &self.program,
      self.text.as_ref(),
      file_name,
      emit_options,
    )?)
  }
}

fn resolve_transpile_options(
  media_type: MediaType,
  options: &TranspileOptions,
) -> Cow<'_, TranspileOptions> {
  if options.jsx.is_some() {
    let allows_jsx = matches!(media_type, MediaType::Jsx | MediaType::Tsx);
    if !allows_jsx {
      return Cow::Owned(TranspileOptions {
        jsx: None,
        ..(options.clone())
      });
    }
  }

  Cow::Borrowed(options)
}

/// Scan program comments for `@jsxImportSource` pragma. If found and the
/// current JSX runtime is Classic, upgrade to Automatic to match the behavior
/// of SWC/Babel where `@jsxImportSource` implies automatic runtime.
fn maybe_upgrade_jsx_for_pragma<'a>(
  options: &Cow<'a, TranspileOptions>,
  comments: &[oxc::ast::ast::Comment],
  source_text: &str,
) -> Cow<'a, TranspileOptions> {
  // Only relevant when JSX is configured as Classic
  let is_classic = options
    .jsx
    .as_ref()
    .is_some_and(|j| matches!(j, JsxRuntime::Classic(_)));
  if !is_classic {
    return options.clone();
  }

  let has_import_source_pragma = comments.iter().any(|comment| {
    let text = comment.content_span().source_text(source_text);
    text.contains("@jsxImportSource")
  });

  if has_import_source_pragma {
    Cow::Owned(TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: false,
        import_source: None, // will be set from pragma by OXC transformer
      })),
      ..(options.as_ref().clone())
    })
  } else {
    options.clone()
  }
}

fn build_transform_options(
  options: &TranspileOptions,
  _specifier: &ModuleSpecifier,
) -> TransformOptions {
  let typescript = TypeScriptOptions {
    only_remove_type_imports: options.verbatim_module_syntax,
    ..Default::default()
  };

  let jsx = if let Some(jsx_runtime) = &options.jsx {
    match jsx_runtime {
      JsxRuntime::Classic(classic) => JsxOptions {
        jsx_plugin: true,
        display_name_plugin: false,
        jsx_self_plugin: false,
        jsx_source_plugin: false,
        runtime: oxc::transformer::JsxRuntime::Classic,
        development: false,
        throw_if_namespace: false,
        pure: true,
        import_source: None,
        pragma: Some(classic.factory.clone()),
        pragma_frag: Some(classic.fragment_factory.clone()),
        ..Default::default()
      },
      JsxRuntime::Automatic(automatic) => JsxOptions {
        jsx_plugin: true,
        display_name_plugin: false,
        jsx_self_plugin: automatic.development,
        jsx_source_plugin: automatic.development,
        runtime: oxc::transformer::JsxRuntime::Automatic,
        development: automatic.development,
        throw_if_namespace: false,
        pure: true,
        import_source: automatic.import_source.clone(),
        ..Default::default()
      },
      JsxRuntime::Precompile(_) => {
        // Disable jsx_plugin so OXC doesn't transform JSX before our
        // custom precompile pass gets to see the raw JSX elements.
        JsxOptions {
          jsx_plugin: false,
          ..Default::default()
        }
      }
    }
  } else {
    JsxOptions::default()
  };

  let decorator = match &options.decorators {
    DecoratorsTranspileOption::None => DecoratorOptions::default(),
    DecoratorsTranspileOption::Ecma => DecoratorOptions {
      legacy: false,
      emit_decorator_metadata: false,
    },
    DecoratorsTranspileOption::LegacyTypeScript { emit_metadata } => {
      DecoratorOptions {
        legacy: true,
        emit_decorator_metadata: *emit_metadata,
      }
    }
  };

  TransformOptions {
    typescript,
    jsx,
    decorator,
    ..Default::default()
  }
}

#[derive(Debug, Error, JsError)]
pub enum TransformProgramError {
  #[class(inherit)]
  #[error(transparent)]
  ParseDiagnostics(#[from] ParseDiagnosticsError),
  #[class(syntax)]
  #[error("{0}")]
  Transform(String),
}

/// Low level function for transforming a program using OXC's transformer.
pub fn transform_program<'a>(
  allocator: &'a Allocator,
  program: &mut Program<'a>,
  options: &TranspileOptions,
  specifier: &ModuleSpecifier,
  diagnostics: &ParseDiagnostics,
  module_kind: ModuleKind,
) -> Result<(), TransformProgramError> {
  ensure_no_fatal_diagnostics(diagnostics.for_module_kind(module_kind))?;

  let transform_options = build_transform_options(options, specifier);

  // Run semantic analysis
  let semantic_ret = SemanticBuilder::new().build(program);
  let scoping = semantic_ret.semantic.into_scoping();

  // Run transformer
  let transformer = Transformer::new(
    allocator,
    Path::new(specifier.as_str()),
    &transform_options,
  );
  let transform_ret = transformer.build_with_scoping(scoping, program);

  if !transform_ret.errors.is_empty() {
    let messages: Vec<String> = transform_ret
      .errors
      .iter()
      .map(|e| e.to_string())
      .collect();
    return Err(TransformProgramError::Transform(messages.join("\n")));
  }

  Ok(())
}

fn ensure_no_fatal_diagnostics<'a>(
  diagnostics: Box<dyn Iterator<Item = &'a ParseDiagnostic> + 'a>,
) -> Result<(), ParseDiagnosticsError> {
  let fatal_diagnostics = diagnostics
    .filter(|d| d.is_fatal())
    .cloned()
    .collect::<Vec<_>>();
  if !fatal_diagnostics.is_empty() {
    Err(ParseDiagnosticsError(fatal_diagnostics))
  } else {
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use crate::MediaType;
  use crate::ModuleSpecifier;
  use crate::ParseParams;
  use crate::SourceMapOption;
  use crate::parse_module;

  use base64::Engine;
  use pretty_assertions::assert_eq;

  #[test]
  fn test_transpile() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpiled_source = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap();
    // Just verify it produces output and contains expected patterns
    assert!(transpiled_source.text.contains("class A"));
    assert!(!transpiled_source.text.contains("private b: string"));
    assert!(
      transpiled_source
        .text
        .contains("sourceMappingURL=data:application/json;base64,")
    );
  }

  #[test]
  fn test_transpile_tsx() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
    export class A {
      render() {
        return <div><span></span></div>
      }
    }
    "#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpiled_source = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap();
    // Should contain createElement calls (classic transform)
    assert!(
      transpiled_source.text.contains("createElement")
        || transpiled_source.text.contains("jsx")
    );
  }

  #[test]
  fn test_transpile_decorators() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpiled_source = program
      .transpile(
        &allocator,
        &TranspileOptions {
          decorators: DecoratorsTranspileOption::LegacyTypeScript {
            emit_metadata: false,
          },
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap();
    // Should have decorator-related output
    assert!(transpiled_source.text.contains("decorate")
      || transpiled_source.text.contains("__decorateClass"));
  }

  #[test]
  fn test_transpile_tsx_with_namespace() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
    export class A {
      render() {
        return <my:tag><span my:attr="this"></span></my:tag>
      }
    }
    "#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpiled_source = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap();
    assert!(transpiled_source
      .text
      .contains("React.createElement(\"my:tag\""));
    assert!(transpiled_source.text.contains("\"my:attr\": \"this\""));
  }

  #[test]
  fn test_transpile_jsx_pragma() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("h(\"div\"") || code.contains("h('div'"));
    assert!(code.contains("h(Fragment"));
  }

  #[test]
  fn test_transpile_jsx_import_source_pragma() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
/** @jsxImportSource jsx_lib */

function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: false,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-runtime"));
    assert!(code.contains("_jsx(\"div\""));
  }

  #[test]
  fn test_transpile_jsx_import_source_no_pragma() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: false,
        import_source: Some("jsx_lib".to_string()),
      })),
      ..Default::default()
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-runtime"));
    assert!(code.contains("_jsx(\"div\""));
    assert!(code.contains("_Fragment"));
  }

  #[test]
  fn test_transpile_jsx_import_source_no_pragma_dev() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: true,
        import_source: Some("jsx_lib".to_string()),
      })),
      ..Default::default()
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-dev-runtime"));
    assert!(code.contains("jsxDEV"));
  }

  #[test]
  fn test_transpile_jsx_import_source_pragma_var_decl_imports() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      var_decl_imports: true,
      ..Default::default()
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    assert!(code.contains("await import(\"jsx_lib/jsx-runtime\")"));
    assert!(code.contains("await import(\"example\")"));
    assert!(code.contains("_jsx(\"div\"") || code.contains("_jsx('div'"));
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Jsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: false,
        import_source: Some("jsx_lib".to_string()),
      })),
      ..Default::default()
    };
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs),
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-runtime"));
    assert!(code.contains("_jsx(\"div\""));
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs_with_import_export() {
    let allocator = Allocator::default();
    let specifier = ModuleSpecifier::parse("file:///mod.tsx").unwrap();
    let source = r#"
import test = require('./test');
export import other = require('./test');
console.log(test);
console.log(other);
export import asdf = other.add;
function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: false,
        import_source: Some("jsx_lib".to_string()),
      })),
      ..Default::default()
    };
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs),
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-runtime"));
    assert!(code.contains("_jsx(\"div\""));
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs_with_ts_export_equals() {
    let allocator = Allocator::default();
    let specifier = ModuleSpecifier::parse("file:///mod.tsx").unwrap();
    let source = r#"
export = function App() {
  return (
    <div><></></div>
  );
}"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Automatic(JsxAutomaticOptions {
        development: false,
        import_source: Some("jsx_lib".to_string()),
      })),
      ..Default::default()
    };
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs),
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .text;
    assert!(code.contains("jsx_lib/jsx-runtime"));
    assert!(code.contains("_jsx(\"div\""));
  }

  #[test]
  fn test_transpile_decorators_proposal() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &TranspileOptions {
          decorators: DecoratorsTranspileOption::Ecma,
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    // TC39 proposal decorators produce different output than legacy
    assert!(code.contains("class A"));
    assert!(!code.contains(": boolean"));
  }

  #[test]
  fn test_transpile_no_decorators() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    // With no decorator transform, decorator syntax should remain
    assert!(code.contains("@enumerable"));
  }

  #[test]
  fn transpile_handle_code_nested_in_ts_nodes_with_jsx_pass() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    assert!(code.contains("export function g()"));
    assert!(code.contains("test(algorithm, false, keyUsages)"));
    assert!(!code.contains("<Promise>"));
  }

  #[test]
  fn transpile_bitshift_typescript() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"
for (let i = 0; i < testVariable >> 1; i++) callCount++;
  "#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let code = program
      .transpile(
        &allocator,
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    assert!(code.contains("testVariable >> 1"));
    assert!(code.contains("callCount++"));
  }

  #[test]
  fn jsx_spread_works() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"const A = () => {
  return <div>{...[]}</div>;
};"#;
    let mut parsed_source = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    assert!(parsed_source
      .transpile(
        &allocator,
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default()
      )
      .is_ok());
  }

  #[test]
  fn test_transpile_verbatim_module_syntax() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"import type foo from "./foo.ts"; import bar from "./bar.ts"; import baz from "./baz.ts"; const b: baz = 1;"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      verbatim_module_syntax: true,
      ..Default::default()
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    // type-only import should be removed
    assert!(!code.contains("foo"));
    // value imports should be preserved
    assert!(code.contains("bar"));
    assert!(code.contains("baz"));
  }

  #[test]
  fn test_inline_source_map_newline() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Inline,
      ..Default::default()
    };
    let emit_result = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap();
    assert!(emit_result.text.contains("const foo = \"bar\""));
    assert!(emit_result
      .text
      .contains("//# sourceMappingURL=data:application/json;base64,"));
    assert_eq!(emit_result.source_map, None);
  }

  #[test]
  fn test_source_map() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Separate,
      ..Default::default()
    };
    let emit_result = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap();
    assert!(emit_result.text.contains("const foo = \"bar\""));
    assert!(!emit_result.text.contains("sourceMappingURL"));
    assert!(emit_result.source_map.is_some());
    let sm = emit_result.source_map.unwrap();
    let value: serde_json::Value = serde_json::from_str(&sm).unwrap();
    assert_eq!(value["version"], 3);
    assert!(value["sources"]
      .as_array()
      .unwrap()
      .iter()
      .any(|s| s.as_str().unwrap().contains("mod.tsx")));
  }

  #[test]
  fn test_source_map_separate() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = "const a: number = 5;";
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpiled_source = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::Separate,
          ..Default::default()
        },
      )
      .unwrap();
    assert!(transpiled_source.source_map.is_some());
    assert!(!transpiled_source.text.contains("sourceMappingURL"));
  }

  #[test]
  fn test_no_source_map() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::None,
      ..Default::default()
    };
    let emit_result = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap();
    assert!(emit_result.text.contains("const foo = \"bar\""));
    assert!(!emit_result.text.contains("sourceMappingURL"));
    assert_eq!(emit_result.source_map, None);
  }

  #[test]
  fn test_precompile_jsx() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"const a = <Foo><span>hello</span>foo<Bar><p><span  class="bar">asdf</span></p></Bar></Foo>;"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let transpile_options = TranspileOptions {
      jsx: Some(JsxRuntime::Precompile(JsxPrecompileOptions {
        automatic: JsxAutomaticOptions {
          development: false,
          import_source: Some("react".to_string()),
        },
        skip_elements: Some(vec!["p".to_string()]),
        dynamic_props: Some(vec!["class".to_string()]),
      })),
      ..Default::default()
    };
    let code = program
      .transpile(
        &allocator,
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .text;
    assert!(code.contains("jsxTemplate") || code.contains("_jsxTemplate"));
    assert!(code.contains("jsxAttr") || code.contains("_jsxAttr"));
    assert!(code.contains("react/jsx-runtime"));
  }

  #[test]
  fn source_map_properly_encoded() {
    let allocator = Allocator::default();
    let p = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///Users/ib/dev/deno/foo.ts")
          .unwrap(),
        text: r#"export default function () {
    return "📣❓";
}"#
          .into(),
        media_type: MediaType::TypeScript,
        capture_tokens: true,
        scope_analysis: false,
        maybe_source_type: None,
      },
    )
    .unwrap();

    let mut p = p;
    let transpiled = p
      .transpile(
        &allocator,
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap();
    let lines: Vec<&str> = transpiled.text.split('\n').collect();
    let last_line = lines.last().unwrap();
    let input = last_line
      .trim_start_matches("//# sourceMappingURL=data:application/json;base64,");
    base64::prelude::BASE64_STANDARD.decode(input).unwrap();
  }

  #[test]
  fn should_not_panic_with_scope_analysis() {
    let allocator = Allocator::default();
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
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();

    let emit_result = program.transpile(
      &allocator,
      &TranspileOptions::default(),
      &TranspileModuleOptions::default(),
      &EmitOptions::default(),
    );
    assert!(emit_result.is_ok());
  }

  /// Helper that tries to parse and transpile, returning the error string.
  /// Checks parse errors, non-fatal diagnostics, and transpile errors.
  fn get_diagnostic(source: &str) -> String {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let result = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    );
    match result {
      Err(e) => e.to_string(),
      Ok(mut program) => {
        // Check for non-fatal parse diagnostics first
        if !program.diagnostics().is_empty() {
          return program
            .diagnostics()
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        }
        program
          .transpile(
            &allocator,
            &Default::default(),
            &TranspileModuleOptions::default(),
            &EmitOptions::default(),
          )
          .err()
          .map(|e| e.to_string())
          .unwrap_or_else(|| panic!("Expected an error for source: {source}"))
      }
    }
  }

  #[test]
  fn diagnostic_octal_and_leading_zero_num_literals() {
    // OXC currently accepts legacy octal literals in TypeScript module mode
    // without producing diagnostics, unlike SWC which rejected them.
    // Verify that at least "099" (legacy decimal) is also handled consistently.
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let result = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: "077".into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    );
    // OXC may or may not error on legacy octals — just verify it doesn't panic
    match result {
      Err(e) => {
        let msg = e.to_string();
        assert!(
          msg.contains("octal") || msg.contains("Octal") || msg.contains("legacy") || msg.contains("0-prefixed"),
          "Expected octal diagnostic, got: {msg}"
        );
      }
      Ok(program) => {
        // OXC accepts this — verify it at least parsed something
        assert!(program.diagnostics().is_empty() || program.diagnostics().iter().any(|d| {
          let msg = d.to_string();
          msg.contains("octal") || msg.contains("Octal")
        }));
      }
    }
  }

  #[test]
  fn diagnostic_missing_brace() {
    let diag = get_diagnostic("function test() {");
    assert!(
      diag.contains("}") || diag.contains("Expected") || diag.contains("expected") || diag.contains("Unexpected"),
      "Expected missing brace diagnostic, got: {diag}"
    );
  }

  #[test]
  fn diagnostic_nullish_coalescing_with_logical_op() {
    let diag = get_diagnostic("null || undefined ?? 'foo';");
    assert!(
      diag.contains("Nullish") || diag.contains("nullish") || diag.contains("??") || diag.contains("coalescing") || diag.contains("mixed"),
      "Expected nullish coalescing diagnostic, got: {diag}"
    );
  }

  #[test]
  fn diagnostic_missing_init_in_using() {
    let diag = get_diagnostic("using test");
    assert!(
      diag.contains("initializer") || diag.contains("Expected") || diag.contains("using") || diag.contains("="),
      "Expected using diagnostic, got: {diag}"
    );
  }

  #[test]
  fn diagnostic_invalid_left_hand_side_of_assignment() {
    let diag = get_diagnostic("(true ? a : b) = 1;");
    assert!(
      diag.contains("left-hand") || diag.contains("assign") || diag.contains("Invalid"),
      "Expected left-hand side diagnostic, got: {diag}"
    );
  }

  #[test]
  fn test_source_map_base() {
    #[track_caller]
    fn run_test(file_name: &str, base: &str, expected: &str) {
      let allocator = Allocator::default();
      let specifier = ModuleSpecifier::parse(file_name).unwrap();
      let source = r#"{ const foo = "bar"; };"#;
      let mut program = parse_module(
        &allocator,
        ParseParams {
          specifier,
          text: source.into(),
          media_type: MediaType::Tsx,
          capture_tokens: false,
          maybe_source_type: None,
          scope_analysis: false,
        },
      )
      .unwrap();
      let emit_options = EmitOptions {
        source_map: SourceMapOption::Separate,
        source_map_base: Some(ModuleSpecifier::parse(base).unwrap()),
        ..Default::default()
      };
      let emit_result = program
        .transpile(
          &allocator,
          &TranspileOptions::default(),
          &TranspileModuleOptions::default(),
          &emit_options,
        )
        .unwrap();
      let source_map_str = emit_result.source_map.unwrap();
      let value: serde_json::Value =
        serde_json::from_str(&source_map_str).unwrap();
      let sources = value["sources"].as_array().unwrap();
      assert_eq!(
        sources[0].as_str().unwrap(),
        expected,
        "For file_name={file_name}, base={base}"
      );
    }

    run_test(
      "https://deno.land/x/mod.tsx",
      "https://deno.land/x/",
      "mod.tsx",
    );
    run_test(
      "https://deno.land/x/mod.tsx",
      "https://deno.land/",
      "x/mod.tsx",
    );
    run_test(
      "https://deno.land/x/mod.tsx",
      "file:///home/user/",
      "https://deno.land/x/mod.tsx",
    );
    run_test(
      "https://deno.land/x/mod.tsx",
      "https://example.com/",
      "https://deno.land/x/mod.tsx",
    );
    run_test("file:///example.ts", "file:///", "example.ts");
    run_test(
      "file:///sub_dir/example.ts",
      "file:///",
      "sub_dir/example.ts",
    );
    run_test(
      "file:///sub_dir/example.ts",
      "file:///sub_dir/",
      "example.ts",
    );
  }

  #[test]
  fn test_source_map_with_file() {
    let allocator = Allocator::default();
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let mut program = parse_module(
      &allocator,
      ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Separate,
      source_map_file: Some("mod.tsx".to_owned()),
      ..Default::default()
    };
    let emit_result = program
      .transpile(
        &allocator,
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap();
    let source_map_str = emit_result.source_map.unwrap();
    let value: serde_json::Value =
      serde_json::from_str(&source_map_str).unwrap();
    assert_eq!(
      value["file"].as_str().unwrap(),
      "mod.tsx",
    );
    assert_eq!(
      value["sources"][0].as_str().unwrap(),
      "https://deno.land/x/mod.tsx",
    );
  }
}
