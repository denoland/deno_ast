// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use crate::swc::ast as swc_ast;
use crate::swc::common::DUMMY_SP;
use crate::swc::visit::noop_fold_type;
use crate::swc::visit::Fold;

/// Transforms import declarations to variable declarations
/// with a dynamic import. This is used to provide import
/// declaration support in script contexts such as the Deno REPL.
pub struct ImportDeclsToVarDeclsFolder;

impl Fold for ImportDeclsToVarDeclsFolder {
  noop_fold_type!(); // skip typescript specific nodes

  fn fold_module_item(
    &mut self,
    module_item: swc_ast::ModuleItem,
  ) -> swc_ast::ModuleItem {
    use crate::swc::ast::*;

    match module_item {
      ModuleItem::ModuleDecl(ModuleDecl::Import(import_decl)) => {
        // Handle type only imports
        if import_decl.type_only {
          // should have no side effects
          return create_empty_stmt();
        }

        // The initializer (ex. `await import('./mod.ts')`)
        let initializer =
          create_await_import_expr(&import_decl.src.value, import_decl.asserts);

        // Handle imports for the side effects
        // ex. `import "module.ts"` -> `await import("module.ts");`
        if import_decl.specifiers.is_empty() {
          return ModuleItem::Stmt(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: initializer,
          }));
        }

        // Collect the specifiers and create the variable statement
        let named_import_props = import_decl
          .specifiers
          .iter()
          .filter_map(|specifier| match specifier {
            ImportSpecifier::Default(specifier) => Some(create_key_value(
              "default".to_string(),
              specifier.local.sym.to_string(),
            )),
            ImportSpecifier::Named(specifier) => {
              Some(match specifier.imported.as_ref() {
                Some(name) => create_key_value(
                  match name {
                    ModuleExportName::Ident(ident) => ident.sym.to_string(),
                    ModuleExportName::Str(str) => str.value.to_string(),
                  },
                  specifier.local.sym.to_string(),
                ),
                None => create_assignment(specifier.local.sym.to_string()),
              })
            }
            ImportSpecifier::Namespace(_) => None,
          })
          .collect::<Vec<_>>();
        let namespace_import_name =
          import_decl
            .specifiers
            .iter()
            .find_map(|specifier| match specifier {
              ImportSpecifier::Namespace(specifier) => {
                Some(create_binding_ident(specifier.local.sym.to_string()))
              }
              _ => None,
            });

        ModuleItem::Stmt(Stmt::Decl(Decl::Var(Box::new(VarDecl {
          span: DUMMY_SP,
          kind: VarDeclKind::Const,
          declare: false,
          decls: {
            let mut decls = Vec::new();

            if !named_import_props.is_empty() {
              decls.push(VarDeclarator {
                span: DUMMY_SP,
                name: Pat::Object(ObjectPat {
                  span: DUMMY_SP,
                  optional: false,
                  props: named_import_props,
                  type_ann: None,
                }),
                definite: false,
                init: Some(initializer.clone()),
              });
            }
            if let Some(namespace_import) = namespace_import_name {
              decls.push(VarDeclarator {
                span: DUMMY_SP,
                name: Pat::Ident(namespace_import),
                definite: false,
                init: Some(initializer),
              });
            }

            decls
          },
        }))))
      }
      _ => module_item,
    }
  }
}

/// Strips export declarations and exports on named exports so the
/// code can be used in script contexts. This is useful for example
/// in the Deno REPL.
pub struct StripExportsFolder;

impl Fold for StripExportsFolder {
  noop_fold_type!(); // skip typescript specific nodes

  fn fold_module_item(
    &mut self,
    module_item: swc_ast::ModuleItem,
  ) -> swc_ast::ModuleItem {
    use crate::swc::ast::*;

    match module_item {
      ModuleItem::ModuleDecl(ModuleDecl::ExportAll(export_all)) => {
        ModuleItem::Stmt(Stmt::Expr(ExprStmt {
          span: DUMMY_SP,
          expr: create_await_import_expr(
            &export_all.src.value,
            export_all.asserts,
          ),
        }))
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportNamed(export_named)) => {
        if let Some(src) = export_named.src {
          ModuleItem::Stmt(Stmt::Expr(ExprStmt {
            span: DUMMY_SP,
            expr: create_await_import_expr(&src.value, export_named.asserts),
          }))
        } else {
          create_empty_stmt()
        }
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultExpr(default_expr)) => {
        // transform a default export expression to its expression
        ModuleItem::Stmt(Stmt::Expr(ExprStmt {
          span: DUMMY_SP,
          expr: default_expr.expr,
        }))
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDecl(export_decl)) => {
        // strip the export keyword on an exported declaration
        ModuleItem::Stmt(Stmt::Decl(export_decl.decl))
      }
      ModuleItem::ModuleDecl(ModuleDecl::ExportDefaultDecl(default_decl)) => {
        // only keep named default exports
        match default_decl.decl {
          DefaultDecl::Fn(FnExpr {
            ident: Some(ident),
            function,
          }) => ModuleItem::Stmt(Stmt::Decl(Decl::Fn(FnDecl {
            declare: false,
            ident,
            function,
          }))),
          DefaultDecl::Class(ClassExpr {
            ident: Some(ident),
            class,
          }) => ModuleItem::Stmt(Stmt::Decl(Decl::Class(ClassDecl {
            declare: false,
            ident,
            class,
          }))),
          _ => create_empty_stmt(),
        }
      }
      _ => module_item,
    }
  }
}

fn create_empty_stmt() -> swc_ast::ModuleItem {
  use swc_ast::*;
  ModuleItem::Stmt(Stmt::Empty(EmptyStmt { span: DUMMY_SP }))
}

fn create_binding_ident(name: String) -> swc_ast::BindingIdent {
  swc_ast::BindingIdent {
    id: create_ident(name),
    type_ann: None,
  }
}

fn create_ident(name: String) -> swc_ast::Ident {
  swc_ast::Ident {
    span: DUMMY_SP,
    sym: name.into(),
    optional: false,
  }
}

fn create_key_value(key: String, value: String) -> swc_ast::ObjectPatProp {
  swc_ast::ObjectPatProp::KeyValue(swc_ast::KeyValuePatProp {
    // use a string literal because it will work in more scenarios than an identifier
    key: swc_ast::PropName::Str(swc_ast::Str {
      span: DUMMY_SP,
      value: key.into(),
      raw: None,
    }),
    value: Box::new(swc_ast::Pat::Ident(swc_ast::BindingIdent {
      id: swc_ast::Ident {
        span: DUMMY_SP,
        sym: value.into(),
        optional: false,
      },
      type_ann: None,
    })),
  })
}

fn create_await_import_expr(
  module_specifier: &str,
  maybe_asserts: Option<Box<swc_ast::ObjectLit>>,
) -> Box<swc_ast::Expr> {
  use swc_ast::*;
  let mut args = vec![ExprOrSpread {
    spread: None,
    expr: Box::new(Expr::Lit(Lit::Str(Str {
      span: DUMMY_SP,
      raw: None,
      value: module_specifier.into(),
    }))),
  }];

  // add assert object if it exists
  if let Some(asserts) = maybe_asserts {
    args.push(ExprOrSpread {
      spread: None,
      expr: Box::new(Expr::Object(ObjectLit {
        span: DUMMY_SP,
        props: vec![PropOrSpread::Prop(Box::new(Prop::KeyValue(
          KeyValueProp {
            key: PropName::Ident(create_ident("assert".to_string())),
            value: Box::new(Expr::Object(*asserts)),
          },
        )))],
      })),
    })
  }

  Box::new(Expr::Await(AwaitExpr {
    span: DUMMY_SP,
    arg: Box::new(Expr::Call(CallExpr {
      span: DUMMY_SP,
      callee: Callee::Expr(Box::new(Expr::Ident(Ident {
        span: DUMMY_SP,
        sym: "import".into(),
        optional: false,
      }))),
      args,
      type_args: None,
    })),
  }))
}

fn create_assignment(key: String) -> swc_ast::ObjectPatProp {
  swc_ast::ObjectPatProp::Assign(swc_ast::AssignPatProp {
    span: DUMMY_SP,
    key: create_ident(key),
    value: None,
  })
}

#[cfg(test)]
mod test {
  use crate::swc::ast::Module;
  use crate::swc::codegen::text_writer::JsWriter;
  use crate::swc::codegen::Node;
  use crate::swc::common::FileName;
  use crate::swc::common::SourceMap;
  use crate::swc::parser::Parser;
  use crate::swc::parser::StringInput;
  use crate::swc::parser::Syntax;
  use crate::swc::parser::TsConfig;
  use crate::swc::visit::Fold;
  use crate::swc::visit::FoldWith;
  use crate::ModuleSpecifier;
  use crate::ES_VERSION;
  use pretty_assertions::assert_eq;
  use std::rc::Rc;

  use super::*;

  #[test]
  fn test_downlevel_imports_type_only() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import type { test } from "./mod.ts";"#,
      ";",
    );
  }

  #[test]
  fn test_downlevel_imports_specifier_only() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import "./mod.ts";"#,
      r#"await import("./mod.ts");"#,
    );

    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import {} from "./mod.ts";"#,
      r#"await import("./mod.ts");"#,
    );
  }

  #[test]
  fn test_downlevel_imports_default() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import mod from "./mod.ts";"#,
      r#"const { "default": mod  } = await import("./mod.ts");"#,
    );
  }

  #[test]
  fn test_downlevel_imports_named() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import { A } from "./mod.ts";"#,
      r#"const { A  } = await import("./mod.ts");"#,
    );

    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import { A, B, C  } from "./mod.ts";"#,
      r#"const { A , B , C  } = await import("./mod.ts");"#,
    );

    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import { A as LocalA, B, C as LocalC  } from "./mod.ts";"#,
      r#"const { "A": LocalA , B , "C": LocalC  } = await import("./mod.ts");"#,
    );
  }

  #[test]
  fn test_downlevel_imports_namespace() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import * as mod from "./mod.ts";"#,
      r#"const mod = await import("./mod.ts");"#,
    );
  }

  #[test]
  fn test_downlevel_imports_mixed() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import myDefault, { A, B as LocalB } from "./mod.ts";"#,
      r#"const { "default": myDefault , A , "B": LocalB  } = await import("./mod.ts");"#,
    );

    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import myDefault, * as mod from "./mod.ts";"#,
      r#"const { "default": myDefault  } = await import("./mod.ts"), mod = await import("./mod.ts");"#,
    );
  }

  #[test]
  fn test_downlevel_imports_assertions() {
    test_transform(
      ImportDeclsToVarDeclsFolder,
      r#"import data from "./mod.json" assert { type: "json" };"#,
      "const { \"default\": data  } = await import(\"./mod.json\", {\n    assert: {\n        type: \"json\"\n    }\n});",
    );
  }

  #[test]
  fn test_strip_exports_export_all() {
    test_transform(
      StripExportsFolder,
      r#"export * from "./test.ts";"#,
      r#"await import("./test.ts");"#,
    );
  }

  #[test]
  fn test_strip_exports_export_named() {
    test_transform(
      StripExportsFolder,
      r#"export { test } from "./test.ts";"#,
      r#"await import("./test.ts");"#,
    );

    test_transform(StripExportsFolder, r#"export { test };"#, ";");
  }

  #[test]
  fn test_strip_exports_assertions() {
    test_transform(
      StripExportsFolder,
      r#"export { default as data } from "./mod.json" assert { type: "json" };"#,
      "await import(\"./mod.json\", {\n    assert: {\n        type: \"json\"\n    }\n});",
    );
  }

  #[test]
  fn test_strip_exports_export_all_assertions() {
    // even though this doesn't really make sense for someone to do
    test_transform(
      StripExportsFolder,
      r#"export * from "./mod.json" assert { type: "json" };"#,
      "await import(\"./mod.json\", {\n    assert: {\n        type: \"json\"\n    }\n});",
    );
  }

  #[test]
  fn test_strip_exports_export_default_expr() {
    test_transform(StripExportsFolder, "export default 5;", "5;");
  }

  #[test]
  fn test_strip_exports_export_default_decl_name() {
    test_transform(
      StripExportsFolder,
      "export default class Test {}",
      "class Test {\n}",
    );

    test_transform(
      StripExportsFolder,
      "export default function test() {}",
      "function test() {}",
    );
  }

  #[test]
  fn test_strip_exports_export_default_decl_no_name() {
    test_transform(StripExportsFolder, "export default class {}", ";");

    test_transform(StripExportsFolder, "export default function() {}", ";");
  }

  #[test]
  fn test_strip_exports_export_named_decls() {
    test_transform(
      StripExportsFolder,
      "export class Test {}",
      "class Test {\n}",
    );

    test_transform(
      StripExportsFolder,
      "export function test() {}",
      "function test() {}",
    );

    test_transform(StripExportsFolder, "export enum Test {}", "enum Test {\n}");

    test_transform(
      StripExportsFolder,
      "export namespace Test {}",
      "module Test {\n}",
    );
  }

  #[test]
  fn test_strip_exports_not_in_namespace() {
    test_transform(
      StripExportsFolder,
      "namespace Test { export class Test {} }",
      "module Test {\n    export class Test {\n    }\n}",
    );
  }

  fn test_transform(
    mut transform: impl Fold,
    src: &str,
    expected_output: &str,
  ) {
    let (source_map, module) = parse(src);
    let output = print(source_map, module.fold_with(&mut transform));
    assert_eq!(output, format!("{}\n", expected_output));
  }

  fn parse(src: &str) -> (Rc<SourceMap>, Module) {
    let source_map = Rc::new(SourceMap::default());
    let source_file = source_map.new_source_file(
      FileName::Url(ModuleSpecifier::parse("file:///test.ts").unwrap()),
      src.to_string(),
    );
    let input = StringInput::from(&*source_file);
    let syntax = Syntax::Typescript(TsConfig {
      ..Default::default()
    });
    let mut parser = Parser::new(syntax, input, None);
    (source_map, parser.parse_module().unwrap())
  }

  fn print(source_map: Rc<SourceMap>, module: Module) -> String {
    let mut buf = vec![];
    {
      let writer =
        Box::new(JsWriter::new(source_map.clone(), "\n", &mut buf, None));
      let config = crate::swc::codegen::Config {
        minify: false,
        ascii_only: false,
        omit_last_semi: false,
        target: ES_VERSION,
      };
      let mut emitter = crate::swc::codegen::Emitter {
        cfg: config,
        comments: None,
        cm: source_map,
        wr: writer,
      };
      module.emit_with(&mut emitter).unwrap();
    }
    String::from_utf8(buf).unwrap()
  }
}
