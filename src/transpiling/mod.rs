// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::rc::Rc;
use std::sync::Arc;

use deno_media_type::MediaType;
use dprint_swc_ext::common::SourceRangedForSpanned;
use dprint_swc_ext::common::SourceTextInfo;
use thiserror::Error;

use crate::emit;
use crate::swc::ast::Program;
use crate::swc::common::chain;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::errors::Diagnostic as SwcDiagnostic;
use crate::swc::parser::error::SyntaxError;
use crate::swc::transforms::fixer;
use crate::swc::transforms::helpers;
use crate::swc::transforms::hygiene;
use crate::swc::transforms::pass::Optional;
use crate::swc::transforms::proposal;
use crate::swc::transforms::react;
use crate::swc::transforms::resolver;
use crate::swc::transforms::typescript;
use crate::swc::visit::as_folder;
use crate::swc::visit::FoldWith;
use crate::EmitError;
use crate::EmitOptions;
use crate::EmittedSourceText;
use crate::Globals;
use crate::Marks;
use crate::ModuleKind;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::ParseDiagnosticsError;
use crate::ParsedSource;
use crate::ProgramRef;
use crate::SourceMap;

use deno_error::JsError;
use std::cell::RefCell;

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
  /// The emit occured consuming the `ParsedSource` without cloning.
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
  #[class(type)]
  #[error("Can't use TranspileOptions::use_decorators_proposal and TranspileOptions::use_ts_decorators together.")]
  DecoratorOptionsConflict,
  /// Parse errors that prevent transpiling.
  #[class(inherit)]
  #[error(transparent)]
  ParseErrors(#[from] ParseDiagnosticsError),
  #[class(inherit)]
  #[error(transparent)]
  FoldProgram(#[from] FoldProgramError),
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

/// Options which can be adjusted when transpiling a module.
///
/// This implements `Hash` so the CLI can use it to bust the emit cache.
#[derive(Debug, Clone, Hash)]
pub struct TranspileOptions {
  /// TypeScript experimental decorators.
  pub use_ts_decorators: bool,

  /// TC39 Decorators Proposal - https://github.com/tc39/proposal-decorators
  pub use_decorators_proposal: bool,

  // Explicit resource management proposal - https://github.com/tc39/proposal-explicit-resource-management
  pub use_explicit_resource_management: bool,

  /// When emitting a legacy decorator, also emit experimental decorator meta
  /// data.  Defaults to `false`.
  pub emit_metadata: bool,
  /// `true` changes type stripping behaviour so that _only_ `type` imports
  /// are stripped.
  pub verbatim_module_syntax: bool,
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
  /// List of elements that should not be precompiled when the JSX precompile
  /// transform is used.
  pub precompile_jsx_skip_elements: Option<Vec<String>>,
  /// List of properties/attributes that should always be treated as
  /// dynamic.
  pub precompile_jsx_dynamic_props: Option<Vec<String>>,
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
      use_explicit_resource_management: false,
      emit_metadata: false,
      verbatim_module_syntax: false,
      imports_not_used_as_values: ImportsNotUsedAsValues::Remove,
      jsx_automatic: false,
      jsx_development: false,
      jsx_factory: "React.createElement".into(),
      jsx_fragment_factory: "React.Fragment".into(),
      jsx_import_source: None,
      transform_jsx: true,
      precompile_jsx: false,
      precompile_jsx_skip_elements: None,
      precompile_jsx_dynamic_props: None,
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
      verbatim_module_syntax: self.verbatim_module_syntax,
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
      native_class_properties: false,
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

impl ParsedSource {
  /// Transform a TypeScript file into a JavaScript file attempting to transpile
  /// owned, then falls back to cloning the program.
  ///
  /// When calling this, you should stirve to only have one reference
  /// to `ParsedSource` in order to prevent cloning.
  pub fn transpile(
    self,
    transpile_options: &TranspileOptions,
    transpile_module_options: &TranspileModuleOptions,
    emit_options: &EmitOptions,
  ) -> Result<TranspileResult, TranspileError> {
    match self.transpile_owned(
      transpile_options,
      transpile_module_options,
      emit_options,
    ) {
      Ok(result) => Ok(TranspileResult::Owned(result?)),
      Err(parsed_source) => {
        // fallback
        parsed_source
          .transpile_cloned(
            transpile_options,
            transpile_module_options,
            emit_options,
          )
          .map(TranspileResult::Cloned)
      }
    }
  }

  fn transpile_cloned(
    &self,
    transpile_options: &TranspileOptions,
    transpile_module_options: &TranspileModuleOptions,
    emit_options: &EmitOptions,
  ) -> Result<EmittedSourceText, TranspileError> {
    let program = (*self.program()).clone();
    transpile(
      self.specifier().clone(),
      self.media_type(),
      self.text().to_string(),
      program,
      // we need the comments to be mutable, so make it single threaded
      self.comments().as_single_threaded(),
      // todo(dsherret): this might be a bug where multiple transpiles could
      // end up with globals from a different transpile, so this should probably
      // do a deep clone of the globals, but that requires changes in swc and this
      // is unlikely to be a problem in practice
      self.globals(),
      &resolve_transpile_options(self.0.media_type, transpile_options),
      transpile_module_options,
      emit_options,
      self.diagnostics(),
    )
  }

  fn transpile_owned(
    self,
    transpile_options: &TranspileOptions,
    transpile_module_options: &TranspileModuleOptions,
    emit_options: &EmitOptions,
  ) -> Result<Result<EmittedSourceText, TranspileError>, ParsedSource> {
    let inner = match Arc::try_unwrap(self.0) {
      Ok(inner) => inner,
      Err(inner) => return Err(ParsedSource(inner)),
    };
    let program = match Arc::try_unwrap(inner.program) {
      Ok(program) => program,
      Err(program) => {
        return Err(ParsedSource(Arc::new(crate::ParsedSourceInner {
          specifier: inner.specifier,
          media_type: inner.media_type,
          text: inner.text,
          source_text_info: inner.source_text_info,
          comments: inner.comments,
          program,
          tokens: inner.tokens,
          syntax_contexts: inner.syntax_contexts,
          diagnostics: inner.diagnostics,
          globals: inner.globals,
        })))
      }
    };
    Ok(transpile(
      inner.specifier,
      inner.media_type,
      inner.text.to_string(),
      program,
      // we need the comments to be mutable, so make it single threaded
      inner.comments.into_single_threaded(),
      &inner.globals,
      &resolve_transpile_options(inner.media_type, transpile_options),
      transpile_module_options,
      emit_options,
      &inner.diagnostics,
    ))
  }
}

fn resolve_transpile_options(
  media_type: MediaType,
  options: &TranspileOptions,
) -> Cow<TranspileOptions> {
  if options.transform_jsx {
    let allows_jsx = matches!(media_type, MediaType::Jsx | MediaType::Tsx);
    if !allows_jsx {
      return Cow::Owned(TranspileOptions {
        // there is no reason for jsx transforms to be turned on because
        // the source cannot possibly allow jsx, so we can get a perf
        // improvement by turning it off
        transform_jsx: false,
        ..(options.clone())
      });
    }
  }

  Cow::Borrowed(options)
}

#[allow(clippy::too_many_arguments)]
fn transpile(
  specifier: ModuleSpecifier,
  media_type: MediaType,
  source: String,
  program: Program,
  comments: SingleThreadedComments,
  globals: &Globals,
  transpile_options: &TranspileOptions,
  transpile_module_options: &TranspileModuleOptions,
  emit_options: &EmitOptions,
  diagnostics: &[ParseDiagnostic],
) -> Result<EmittedSourceText, TranspileError> {
  if transpile_options.use_decorators_proposal
    && transpile_options.use_ts_decorators
  {
    return Err(TranspileError::DecoratorOptionsConflict);
  }

  let module_kind = transpile_module_options.module_kind.unwrap_or({
    if matches!(media_type, MediaType::Cjs | MediaType::Cts) {
      ModuleKind::Cjs
    } else {
      ModuleKind::Esm
    }
  });
  let program = match program {
    Program::Module(module) => match module_kind {
      ModuleKind::Cjs if ProgramRef::Module(&module).compute_is_script() => {
        Program::Script(convert_script_module_to_swc_script(
          &specifier, &source, module,
        )?)
      }
      _ => Program::Module(module),
    },
    Program::Script(script) => {
      match module_kind {
        ModuleKind::Esm => {
          // force transpiling as a module so that the jsx import source is
          // injected as an import declaration and not require
          Program::Module(crate::swc::ast::Module {
            span: script.span,
            body: script
              .body
              .into_iter()
              .map(crate::swc::ast::ModuleItem::Stmt)
              .collect(),
            shebang: script.shebang,
          })
        }
        ModuleKind::Cjs => Program::Script(script),
      }
    }
  };

  let source_map = SourceMap::single(specifier, source);
  let program = globals.with(|marks| {
    fold_program(
      program,
      transpile_options,
      &source_map,
      &comments,
      marks,
      diagnostics,
    )
  })?;

  Ok(emit(
    (&program).into(),
    &comments,
    &source_map,
    emit_options,
  )?)
}

/// swc treats any file with `import X = require("...")` or `export = ...` as a
/// module, but we want these to be transpiled as a script with stuff like JSX import
/// sources being imported with a require call, so we convert it here to a script
/// before transpiling
fn convert_script_module_to_swc_script(
  specifier: &ModuleSpecifier,
  source: &str,
  module: crate::swc::ast::Module,
) -> Result<crate::swc::ast::Script, TranspileError> {
  use crate::swc::ast::*;
  use crate::swc::common::SyntaxContext;
  use crate::swc::common::DUMMY_SP;

  fn ts_entity_name_to_expr(ts_entity_name: TsEntityName) -> Expr {
    match ts_entity_name {
      TsEntityName::Ident(ident) => Expr::Ident(ident),
      TsEntityName::TsQualifiedName(ts_qualified_name) => {
        ts_qualified_name_to_expr(*ts_qualified_name)
      }
    }
  }

  fn ts_qualified_name_to_expr(ts_qualified_name: TsQualifiedName) -> Expr {
    match ts_qualified_name.left {
      TsEntityName::Ident(ident) => Expr::Member(MemberExpr {
        span: DUMMY_SP,
        obj: Box::new(Expr::Ident(ident)),
        prop: MemberProp::Ident(IdentName::new(
          ts_qualified_name.right.sym,
          ts_qualified_name.right.span,
        )),
      }),
      TsEntityName::TsQualifiedName(ts_qualified_name) => {
        Expr::Member(MemberExpr {
          span: DUMMY_SP,
          obj: Box::new(ts_entity_name_to_expr(ts_qualified_name.left)),
          prop: MemberProp::Ident(IdentName::new(
            ts_qualified_name.right.sym,
            ts_qualified_name.right.span,
          )),
        })
      }
    }
  }

  fn module_decl_to_stmt(
    specifier: &ModuleSpecifier,
    source: &str,
    decl: ModuleDecl,
    stmts: &mut Vec<Stmt>,
  ) -> Result<(), TranspileError> {
    match decl {
      ModuleDecl::Import(_)
      | ModuleDecl::ExportDecl(_)
      | ModuleDecl::ExportNamed(_)
      | ModuleDecl::ExportDefaultDecl(_)
      | ModuleDecl::ExportDefaultExpr(_)
      | ModuleDecl::ExportAll(_) => {
        return Err(TranspileError::ParseErrors(ParseDiagnosticsError(vec![
          ParseDiagnostic {
            specifier: specifier.clone(),
            range: decl.range(),
            kind: SyntaxError::ImportExportInScript,
            source: SourceTextInfo::new(source.into()),
          },
        ])));
      }
      ModuleDecl::TsImportEquals(ts_import_equals_decl) => {
        if ts_import_equals_decl.is_type_only {
          return Ok(());
        }
        let export_ident = if ts_import_equals_decl.is_export {
          Some(ts_import_equals_decl.id.clone())
        } else {
          None
        };
        let var_decl = VarDecl {
          span: ts_import_equals_decl.span,
          ctxt: ts_import_equals_decl.id.ctxt,
          kind: VarDeclKind::Const,
          declare: false,
          decls: Vec::from([VarDeclarator {
            span: ts_import_equals_decl.span,
            name: ts_import_equals_decl.id.into(),
            init: Some(match ts_import_equals_decl.module_ref {
              TsModuleRef::TsEntityName(ts_entity_name) => {
                Box::new(ts_entity_name_to_expr(ts_entity_name))
              }
              TsModuleRef::TsExternalModuleRef(ts_external_module_ref) => {
                // call expr for require(...)
                Box::new(Expr::Call(CallExpr {
                  span: DUMMY_SP,
                  callee: Callee::Expr(Box::new(Expr::Ident(Ident::new(
                    "require".into(),
                    DUMMY_SP,
                    SyntaxContext::empty(),
                  )))),
                  ctxt: SyntaxContext::empty(),
                  args: vec![ExprOrSpread {
                    expr: Box::new(Expr::Lit(Lit::Str(Str {
                      span: ts_external_module_ref.span,
                      value: ts_external_module_ref.expr.value.clone(),
                      raw: None,
                    }))),
                    spread: None,
                  }],
                  type_args: None,
                }))
              }
            }),
            definite: false,
          }]),
        };
        stmts.push(Stmt::Decl(Decl::Var(Box::new(var_decl))));
        if let Some(export_ident) = export_ident {
          stmts.push(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: Box::new(Expr::Assign(AssignExpr {
              span: DUMMY_SP,
              op: AssignOp::Assign,
              left: AssignTarget::Simple(SimpleAssignTarget::Member(
                MemberExpr {
                  span: DUMMY_SP,
                  obj: Box::new(Expr::Ident(Ident {
                    span: DUMMY_SP,
                    ctxt: export_ident.ctxt,
                    sym: "exports".into(),
                    optional: false,
                  })),
                  prop: MemberProp::Ident(IdentName::new(
                    export_ident.sym.clone(),
                    export_ident.span,
                  )),
                },
              )),
              right: Box::new(Expr::Ident(Ident::new(
                export_ident.sym,
                export_ident.span,
                export_ident.ctxt,
              ))),
            })),
          }));
        }
      }
      ModuleDecl::TsExportAssignment(ts_export_assignment) => {
        // convert `export = ...` to `module.exports = ...`
        stmts.push(Stmt::Expr(ExprStmt {
          span: DUMMY_SP,
          expr: Box::new(Expr::Assign(AssignExpr {
            span: DUMMY_SP,
            op: AssignOp::Assign,
            left: AssignTarget::Simple(SimpleAssignTarget::Member(
              MemberExpr {
                span: DUMMY_SP,
                obj: Box::new(Expr::Ident(Ident::new(
                  "module".into(),
                  DUMMY_SP,
                  SyntaxContext::empty(),
                ))),
                prop: MemberProp::Ident(IdentName::new(
                  "exports".into(),
                  DUMMY_SP,
                )),
              },
            )),
            right: ts_export_assignment.expr,
          })),
        }))
      }
      ModuleDecl::TsNamespaceExport(_) => {
        // ignore as it's type only
      }
    }

    Ok(())
  }

  let module_decl_count = module
    .body
    .iter()
    .filter(|m| matches!(m, ModuleItem::ModuleDecl(_)))
    .count();
  // a module decl could have two statements when an export, so reserve some extra capacity
  let mut stmts = Vec::with_capacity(module.body.len() + module_decl_count);
  for module_item in module.body {
    match module_item {
      ModuleItem::Stmt(stmt) => {
        stmts.push(stmt);
      }
      ModuleItem::ModuleDecl(decl) => {
        module_decl_to_stmt(specifier, source, decl, &mut stmts)?;
      }
    }
  }

  Ok(Script {
    span: module.span,
    body: stmts,
    shebang: module.shebang,
  })
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

#[derive(Debug, Error, JsError)]
pub enum FoldProgramError {
  #[class(inherit)]
  #[error(transparent)]
  ParseDiagnostics(#[from] ParseDiagnosticsError),
  #[class(inherit)]
  #[error(transparent)]
  Swc(#[from] SwcFoldDiagnosticsError),
}

/// Low level function for transpiling a program.
pub fn fold_program(
  program: Program,
  options: &TranspileOptions,
  source_map: &SourceMap,
  comments: &SingleThreadedComments,
  marks: &Marks,
  diagnostics: &[ParseDiagnostic],
) -> Result<Program, FoldProgramError> {
  ensure_no_fatal_diagnostics(diagnostics)?;

  let mut passes = chain!(
    Optional::new(transforms::StripExportsFolder, options.var_decl_imports),
    resolver(marks.unresolved, marks.top_level, true),
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
    Optional::new(
      proposal::explicit_resource_management::explicit_resource_management(),
      options.use_explicit_resource_management
    ),
    helpers::inject_helpers(marks.top_level),
    // transform imports to var decls before doing the typescript pass
    // so that swc doesn't do any optimizations on the import declarations
    Optional::new(
      transforms::ImportDeclsToVarDeclsFolder,
      options.var_decl_imports
    ),
    Optional::new(
      typescript::typescript(
        options.as_typescript_config(),
        marks.unresolved,
        marks.top_level
      ),
      !options.transform_jsx
    ),
    Optional::new(
      typescript::tsx(
        source_map.inner().clone(),
        options.as_typescript_config(),
        options.as_tsx_config(),
        comments,
        marks.unresolved,
        marks.top_level,
      ),
      options.transform_jsx
    ),
    Optional::new(
      as_folder(jsx_precompile::JsxPrecompile::new(
        options.jsx_import_source.clone().unwrap_or_default(),
        options.precompile_jsx_skip_elements.clone(),
        options.precompile_jsx_dynamic_props.clone(),
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
        marks.top_level,
        marks.unresolved,
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

#[derive(Debug, JsError)]
#[class(syntax)]
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
        // invalid left hand side of assignment
        SyntaxError::TS2406 |
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

  use crate::parse_program;
  use crate::MediaType;
  use crate::ModuleSpecifier;
  use crate::ParseParams;
  use crate::SourceMapOption;

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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpiled_source = program
      .transpile(
        &TranspileOptions {
          use_explicit_resource_management: true,
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source();
    let expected_text = r#"function _using_ctx() {
  var _disposeSuppressedError = typeof SuppressedError === "function" ? SuppressedError : function(error, suppressed) {
    var err = new Error();
    err.name = "SuppressedError";
    err.suppressed = suppressed;
    err.error = error;
    return err;
  }, empty = {}, stack = [];
  function using(isAwait, value) {
    if (value != null) {
      if (Object(value) !== value) {
        throw new TypeError("using declarations can only be used with objects, functions, null, or undefined.");
      }
      if (isAwait) {
        var dispose = value[Symbol.asyncDispose || Symbol.for("Symbol.asyncDispose")];
      }
      if (dispose == null) {
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
    } else if (isAwait) {
      stack.push({
        d: value,
        a: isAwait
      });
    }
    return value;
  }
  return {
    e: empty,
    u: using.bind(null, false),
    a: using.bind(null, true),
    d: function() {
      var error = this.e;
      function next() {
        while(resource = stack.pop()){
          try {
            var resource, disposalResult = resource.d && resource.d.call(resource.v);
            if (resource.a) {
              return Promise.resolve(disposalResult).then(next, err);
            }
          } catch (e) {
            return err(e);
          }
        }
        if (error !== empty) throw error;
      }
      function err(e) {
        error = error !== empty ? new _disposeSuppressedError(error, e) : e;
        return next();
      }
      return next();
    }
  };
}
try {
  var _usingCtx = _using_ctx();
  var data = _usingCtx.u(create());
  console.log(data);
} catch (_) {
  _usingCtx.e = _;
} finally{
  _usingCtx.d();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true, // ensure scope analysis doesn't conflict with a second resolver pass
    })
    .unwrap();
    let transpiled_source = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Jsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();
    let code = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: false,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
      .text;
    let expected = r#"/** @jsxImportSource jsx_lib */ const { "jsx": _jsx, "Fragment": _Fragment } = await import("jsx_lib/jsx-runtime");
const example = await import("example");
function App() {
  return /*#__PURE__*/ _jsx("div", {
    children: /*#__PURE__*/ _jsx(_Fragment, {})
  });
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"
function App() {
  return (
    <div><></></div>
  );
}"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs), // notice this
    };
    let code = program
      .transpile(
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
      .text;
    let expected = r#"const { jsx: _jsx, Fragment: _Fragment } = require("jsx_lib/jsx-runtime");
function App() {
  return _jsx("div", {
    children: _jsx(_Fragment, {})
  });
}
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs_with_import_export() {
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
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
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs), // notice this
    };
    let code = program
      .transpile(
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
      .text;
    let expected = r#"const { jsx: _jsx, Fragment: _Fragment } = require("jsx_lib/jsx-runtime");
const test = require("./test");
const other = require("./test");
exports.other = other;
console.log(test);
console.log(other);
const asdf = other.add;
exports.asdf = asdf;
function App() {
  return _jsx("div", {
    children: _jsx(_Fragment, {})
  });
}
"#;
    assert_eq!(&code[..expected.len()], expected);
  }

  #[test]
  fn test_transpile_jsx_import_source_cjs_with_ts_export_equals() {
    let specifier = ModuleSpecifier::parse("file:///mod.tsx").unwrap();
    let source = r#"
export = function App() {
  return (
    <div><></></div>
  );
}"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
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
    let transpile_module_options = TranspileModuleOptions {
      module_kind: Some(ModuleKind::Cjs), // notice this
    };
    let code = program
      .transpile(
        &transpile_options,
        &transpile_module_options,
        &EmitOptions {
          remove_comments: true,
          ..Default::default()
        },
      )
      .unwrap()
      .into_source()
      .text;
    let expected = r#"const { jsx: _jsx, Fragment: _Fragment } = require("jsx_lib/jsx-runtime");
module.exports = function App() {
  return _jsx("div", {
    children: _jsx(_Fragment, {})
  });
};
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = program
      .transpile(
        &TranspileOptions {
          use_ts_decorators: true,
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = program
      .transpile(
        &TranspileOptions {
          use_decorators_proposal: true,
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    program
      .transpile(
        &TranspileOptions {
          use_decorators_proposal: true,
          use_ts_decorators: true,
          ..Default::default()
        },
        &TranspileModuleOptions::default(),
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let code = program
      .transpile(
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
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
    let parsed_source = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    assert!(parsed_source
      .transpile(
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default()
      )
      .is_ok());
  }

  #[test]
  fn diagnostic_octal_and_leading_zero_num_literals() {
    // todo(dsherret): remove export {} after https://github.com/swc-project/swc/pull/9682
    assert_eq!(get_diagnostic("077;\n\nexport {};"), concat!(
      "Legacy octal literals are not available when targeting ECMAScript 5 and higher ",
      "at https://deno.land/x/mod.ts:1:1\n\n",
      "  077;\n",
      "  ~~~\n\n",
      "Legacy octal escape is not permitted in strict mode at https://deno.land/x/mod.ts:1:1\n\n",
      "  077;\n",
      "  ~~~",
    ));
    assert_eq!(get_diagnostic("099;\n\nexport {};"), concat!(
      "Legacy decimal escape is not permitted in strict mode at https://deno.land/x/mod.ts:1:1\n\n",
      "  099;\n",
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

  #[test]
  fn diagnostic_invalid_left_hand_side_of_assignment() {
    assert_eq!(get_diagnostic("(true ? a : b) = 1;"), concat!(
      "The left-hand side of an assignment expression must be a variable or a property access. at https://deno.land/x/mod.ts:1:1\n\n",
      "  (true ? a : b) = 1;\n",
      "  ~~~~~~~~~~~~~~\n",
      "\n",
      // for some reason, swc does the same diagnostic twice
      "The left-hand side of an assignment expression must be a variable or a property access. at https://deno.land/x/mod.ts:1:1\n\n",
      "  (true ? a : b) = 1;\n",
      "  ~~~~~~~~~~~~~~",
    ));
  }

  fn get_diagnostic(source: &str) -> String {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let parsed_source = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    parsed_source
      .transpile(
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .err()
      .unwrap()
      .to_string()
  }

  #[test]
  fn source_map_properly_encoded() {
    // Ref https://github.com/denoland/deno/issues/10936
    // Ref https://github.com/swc-project/swc/issues/3288#issuecomment-1117252904

    let p = parse_program(ParseParams {
      specifier: ModuleSpecifier::parse("file:///Users/ib/dev/deno/foo.ts")
        .unwrap(),
      text: r#"export default function () {
    return "📣❓";
}"#
        .into(),
      media_type: MediaType::TypeScript,
      capture_tokens: true,
      scope_analysis: false,
      maybe_syntax: None,
    })
    .unwrap();

    let transpiled = p
      .transpile(
        &Default::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source();
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
    let source = r#"const a = <Foo><span>hello</span>foo<Bar><p><span  class="bar">asdf</span></p></Bar></Foo>;"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      transform_jsx: false,
      precompile_jsx: true,
      precompile_jsx_skip_elements: Some(vec!["p".to_string()]),
      precompile_jsx_dynamic_props: Some(vec!["class".to_string()]),
      jsx_import_source: Some("react".to_string()),
      ..Default::default()
    };
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
      .text;
    let expected1 = r#"import { jsx as _jsx, jsxTemplate as _jsxTemplate, jsxAttr as _jsxAttr } from "react/jsx-runtime";
const $$_tpl_2 = [
  "<span ",
  ">asdf</span>"
];
const $$_tpl_1 = [
  "<span>hello</span>foo",
  ""
];
const a = _jsx(Foo, {
  children: _jsxTemplate($$_tpl_1, _jsx(Bar, {
    children: _jsx("p", {
      children: _jsxTemplate($$_tpl_2, _jsxAttr("class", "bar"))
    })
  }))
});
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2Vz"#;
    assert_eq!(&code[0..expected1.len()], expected1);
  }

  #[test]
  fn test_verbatim_module_syntax() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.ts").unwrap();
    let source = r#"import type foo from "./foo.ts"; import bar from "./bar.ts"; import baz from "./baz.ts"; const b: baz = 1;"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let transpile_options = TranspileOptions {
      verbatim_module_syntax: true,
      ..Default::default()
    };
    let code = program
      .transpile(
        &transpile_options,
        &TranspileModuleOptions::default(),
        &EmitOptions::default(),
      )
      .unwrap()
      .into_source()
      .text;
    let expected1 = r#"import bar from "./bar.ts";
import baz from "./baz.ts";
const b = 1;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbImh0dHBzOi8vZGVuby5sYW5kL3gvbW9kLnRzIl0sInNvdXJjZXNDb250ZW50IjpbImltcG9ydCB0eXBlIGZvbyBmcm9tIFwiLi9mb28udHNcIjsgaW1wb3J0IGJhciBmcm9tIFwiLi9iYXIudHNcIjsgaW1wb3J0IGJheiBmcm9tIFwiLi9iYXoudHNcIjsgY29uc3QgYjogYmF6ID0gMTsiXSwibmFtZXMiOltdLCJtYXBwaW5ncyI6IkFBQWlDLE9BQU8sU0FBUyxXQUFXO0FBQUMsT0FBTyxTQUFTLFdBQVc7QUFB"#;
    assert_eq!(&code[0..expected1.len()], expected1);
  }

  #[test]
  fn test_inline_source_map_newline() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let emit_result = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap()
      .into_source();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let emit_result = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap()
      .into_source();
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
  fn test_source_map_base() {
    #[track_caller]
    fn run_test(file_name: &str, base: &str, expected: &str) {
      let specifier = ModuleSpecifier::parse(file_name).unwrap();
      let source = r#"{ const foo = "bar"; };"#;
      let program = parse_program(ParseParams {
        specifier,
        text: source.into(),
        media_type: MediaType::Tsx,
        capture_tokens: false,
        maybe_syntax: None,
        scope_analysis: false,
      })
      .unwrap();
      let emit_options = EmitOptions {
        source_map: SourceMapOption::Separate,
        source_map_base: Some(ModuleSpecifier::parse(base).unwrap()),
        ..Default::default()
      };
      let emit_result = program
        .transpile(
          &TranspileOptions::default(),
          &TranspileModuleOptions::default(),
          &emit_options,
        )
        .unwrap()
        .into_source();
      assert_eq!(
        &emit_result.text,
        r#"{
  const foo = "bar";
}"#
      );
      let value: serde_json::Value =
        serde_json::from_str(&emit_result.source_map.unwrap()).unwrap();
      assert_eq!(
        value,
        serde_json::json!({
          "version":3,
          "sources":[expected],
          "sourcesContent":["{ const foo = \"bar\"; };"],
          "names":[],
          "mappings":"AAAA;EAAE,MAAM,MAAM;AAAO"
        })
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
    run_test(
      "https://example.com/base/",
      "https://example.com/base/",
      // maybe not ideal, but this is ok
      "https://example.com/base/",
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
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::Tsx,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_options = EmitOptions {
      source_map: SourceMapOption::Separate,
      source_map_file: Some("mod.tsx".to_owned()),
      ..Default::default()
    };
    let emit_result = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap()
      .into_source();
    assert_eq!(
      &emit_result.text,
      r#"{
  const foo = "bar";
}"#
    );
    assert_eq!(
      emit_result.source_map.as_deref(),
      Some(
        r#"{"version":3,"file":"mod.tsx","sources":["https://deno.land/x/mod.tsx"],"sourcesContent":["{ const foo = \"bar\"; };"],"names":[],"mappings":"AAAA;EAAE,MAAM,MAAM;AAAO"}"#
      )
    );
  }

  #[test]
  fn test_no_source_map() {
    let specifier =
      ModuleSpecifier::parse("https://deno.land/x/mod.tsx").unwrap();
    let source = r#"{ const foo = "bar"; };"#;
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
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
    let emit_result = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &emit_options,
      )
      .unwrap()
      .into_source();
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    let emit_result = program
      .transpile_owned(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();

    // won't transpile since the program is cloned
    let borrowed_program = program.clone();
    let result = program.transpile_owned(
      &Default::default(),
      &Default::default(),
      &Default::default(),
    );
    let program = result.err().unwrap();
    drop(borrowed_program);

    // won't transpile since the program is cloned
    let borrowed_program = program.program().clone();
    let result = program.transpile_owned(
      &Default::default(),
      &Default::default(),
      &Default::default(),
    );
    let program = result.err().unwrap();
    drop(borrowed_program);

    // now it will work
    let emit_result = program
      .transpile_owned(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();

    // even though it's borrowed, it will transpile
    let borrowed_program = program.clone();
    let emit_result = program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap();
    let emit_result = match emit_result {
      TranspileResult::Owned(_) => unreachable!(),
      TranspileResult::Cloned(emit_result) => emit_result,
    };
    assert_eq!(&emit_result.text, "const foo = \"bar\";\n");

    // now it's owned, should still work
    let emit_result = borrowed_program
      .transpile(
        &TranspileOptions::default(),
        &TranspileModuleOptions::default(),
        &EmitOptions {
          source_map: SourceMapOption::None,
          ..Default::default()
        },
      )
      .unwrap();
    let emit_result = match emit_result {
      TranspileResult::Cloned(_) => unreachable!(),
      TranspileResult::Owned(emit_result) => emit_result,
    };
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
    let program = parse_program(ParseParams {
      specifier,
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();

    let emit_result = program.transpile(
      &TranspileOptions::default(),
      &TranspileModuleOptions::default(),
      &EmitOptions::default(),
    );
    assert!(emit_result.is_ok());
  }
}
