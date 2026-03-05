// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use oxc::allocator::Allocator;
use oxc::allocator::CloneIn;
use oxc::ast::ast::*;
use oxc::ast::AstBuilder;
use oxc::ast_visit::walk_mut;
use oxc::ast_visit::VisitMut;
use oxc::span::SPAN;

/// Transforms import declarations to variable declarations
/// with a dynamic import. This is used to provide import
/// declaration support in script contexts such as the Deno REPL.
pub struct ImportDeclsToVarDecls<'a> {
  ast: AstBuilder<'a>,
}

impl<'a> ImportDeclsToVarDecls<'a> {
  pub fn new(allocator: &'a Allocator) -> Self {
    Self {
      ast: AstBuilder::new(allocator),
    }
  }
}

impl<'a> VisitMut<'a> for ImportDeclsToVarDecls<'a> {
  fn visit_statements(&mut self, stmts: &mut oxc::allocator::Vec<'a, Statement<'a>>) {
    let mut new_stmts = self.ast.vec_with_capacity(stmts.len());
    for stmt in stmts.drain(..) {
      match stmt {
        Statement::ImportDeclaration(import_decl) => {
          if import_decl.import_kind.is_type() {
            // type-only import - remove entirely
            continue;
          }

          let source_value = import_decl.source.value.as_str();

          // Side-effect only import: `import "./mod.ts"` -> `await import("./mod.ts")`
          if import_decl.specifiers.as_ref().map_or(true, |s| s.is_empty()) {
            let import_expr = self.create_await_import_expr(source_value);
            new_stmts.push(self.ast.statement_expression(SPAN, import_expr));
            continue;
          }

          let specifiers = import_decl.specifiers.as_ref().unwrap();

          // Collect named/default imports for destructuring
          let mut props = self.ast.vec();
          let mut namespace_name: Option<&str> = None;

          for specifier in specifiers {
            match specifier {
              ImportDeclarationSpecifier::ImportDefaultSpecifier(s) => {
                let key = self.ast.property_key_static_identifier(SPAN, "default");
                let value = self.ast.binding_pattern_binding_identifier(SPAN, s.local.name.as_str());
                props.push(self.ast.binding_property(SPAN, key, value, false, false));
              }
              ImportDeclarationSpecifier::ImportSpecifier(s) => {
                let imported_name = s.imported.name();
                let local_name = s.local.name.as_str();
                let shorthand = imported_name == local_name;
                let key = self.ast.property_key_static_identifier(SPAN, imported_name);
                let value = self.ast.binding_pattern_binding_identifier(SPAN, local_name);
                props.push(self.ast.binding_property(SPAN, key, value, shorthand, false));
              }
              ImportDeclarationSpecifier::ImportNamespaceSpecifier(s) => {
                namespace_name = Some(s.local.name.as_str());
              }
            }
          }

          let import_expr = self.create_await_import_expr(source_value);

          let mut declarators = self.ast.vec();

          if !props.is_empty() {
            let pattern = self.ast.binding_pattern_object_pattern(SPAN, props, None::<BindingRestElement<'a>>);
            declarators.push(self.ast.variable_declarator(
              SPAN,
              VariableDeclarationKind::Const,
              pattern,
              None::<TSTypeAnnotation<'a>>,
              Some(import_expr.clone_in(self.ast.allocator)),
              false,
            ));
          }

          if let Some(ns_name) = namespace_name {
            let pattern = self.ast.binding_pattern_binding_identifier(SPAN, ns_name);
            declarators.push(self.ast.variable_declarator(
              SPAN,
              VariableDeclarationKind::Const,
              pattern,
              None::<TSTypeAnnotation<'a>>,
              Some(import_expr),
              false,
            ));
          }

          if !declarators.is_empty() {
            let var_decl = Statement::from(
              self.ast.declaration_variable(SPAN, VariableDeclarationKind::Const, declarators, false),
            );
            new_stmts.push(var_decl);
          }
        }
        _ => {
          new_stmts.push(stmt);
        }
      }
    }
    *stmts = new_stmts;

    // Visit children
    walk_mut::walk_statements(self, stmts);
  }
}

impl<'a> ImportDeclsToVarDecls<'a> {
  fn create_await_import_expr(&self, source: &str) -> Expression<'a> {
    let source_in_alloc = oxc::allocator::StringBuilder::from_str_in(source, self.ast.allocator);
    let import_arg = self.ast.expression_string_literal(SPAN, source_in_alloc.into_str(), None);
    let import_call = self.ast.expression_import(SPAN, import_arg, None, None);
    self.ast.expression_await(SPAN, import_call)
  }
}

/// Strips export declarations and exports on named exports so the
/// code can be used in script contexts. This is useful for example
/// in the Deno REPL.
pub struct StripExports<'a> {
  ast: AstBuilder<'a>,
}

impl<'a> StripExports<'a> {
  pub fn new(allocator: &'a Allocator) -> Self {
    Self {
      ast: AstBuilder::new(allocator),
    }
  }
}

impl<'a> VisitMut<'a> for StripExports<'a> {
  fn visit_statements(&mut self, stmts: &mut oxc::allocator::Vec<'a, Statement<'a>>) {
    let mut new_stmts = self.ast.vec_with_capacity(stmts.len());
    for stmt in stmts.drain(..) {
      match stmt {
        Statement::ExportAllDeclaration(export_all) => {
          let source_value = export_all.source.value.as_str();
          let import_expr = self.create_await_import_expr(source_value);
          new_stmts.push(self.ast.statement_expression(SPAN, import_expr));
        }
        Statement::ExportNamedDeclaration(export_named) => {
          if let Some(source) = &export_named.source {
            let source_value = source.value.as_str();
            let import_expr = self.create_await_import_expr(source_value);
            new_stmts.push(self.ast.statement_expression(SPAN, import_expr));
          } else if let Some(decl) = export_named.unbox().declaration {
            new_stmts.push(Statement::from(decl));
          }
          // `export { test }` -> remove entirely
        }
        Statement::ExportDefaultDeclaration(export_default) => {
          match export_default.unbox().declaration {
            ExportDefaultDeclarationKind::FunctionDeclaration(func) => {
              if func.id.is_some() {
                new_stmts.push(Statement::FunctionDeclaration(func));
              }
            }
            ExportDefaultDeclarationKind::ClassDeclaration(class) => {
              if class.id.is_some() {
                new_stmts.push(Statement::ClassDeclaration(class));
              }
            }
            ExportDefaultDeclarationKind::TSInterfaceDeclaration(_) => {
              // type only - discard
            }
            kind => {
              // `export default <expr>` -> `<expr>;`
              // ExportDefaultDeclarationKind inherits Expression variants,
              // so all remaining variants are expressions.
              if kind.is_expression() {
                let expr = kind.into_expression();
                new_stmts.push(self.ast.statement_expression(SPAN, expr));
              }
            }
          }
        }
        _ => {
          new_stmts.push(stmt);
        }
      }
    }
    *stmts = new_stmts;

    walk_mut::walk_statements(self, stmts);
  }
}

impl<'a> StripExports<'a> {
  fn create_await_import_expr(&self, source: &str) -> Expression<'a> {
    let source_in_alloc = oxc::allocator::StringBuilder::from_str_in(source, self.ast.allocator);
    let import_arg = self.ast.expression_string_literal(SPAN, source_in_alloc.into_str(), None);
    let import_call = self.ast.expression_import(SPAN, import_arg, None, None);
    self.ast.expression_await(SPAN, import_call)
  }
}
