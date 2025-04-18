// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use crate::swc::ast::Id;
use crate::swc::ast::{
  ArrowExpr, BlockStmt, BlockStmtOrExpr, CatchClause, ClassDecl, ClassExpr,
  DoWhileStmt, Expr, FnDecl, FnExpr, ForInStmt, ForOfStmt, ForStmt, Function,
  Ident, ImportDefaultSpecifier, ImportNamedSpecifier, ImportStarAsSpecifier,
  Param, Pat, SwitchStmt, TsInterfaceDecl, TsTypeAliasDecl, VarDecl,
  VarDeclKind, WhileStmt, WithStmt,
};
use crate::swc::atoms::Atom;
use crate::swc::ecma_visit::Visit;
use crate::swc::ecma_visit::VisitWith;
use crate::swc::utils::find_pat_ids;
use crate::view;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Scope {
  vars: HashMap<Id, Var>,
  symbols: HashMap<Atom, Vec<Id>>,
}

impl Scope {
  pub fn analyze(program: view::Program) -> Self {
    let mut scope = Self {
      vars: Default::default(),
      symbols: Default::default(),
    };
    let mut path = vec![];

    match program {
      view::Program::Module(module) => {
        module.inner.visit_with(&mut Analyzer {
          scope: &mut scope,
          path: &mut path,
        });
      }
      view::Program::Script(script) => {
        script.inner.visit_with(&mut Analyzer {
          scope: &mut scope,
          path: &mut path,
        });
      }
    };

    scope
  }

  // Get all declarations with a symbol.
  pub fn ids_with_symbol(&self, sym: &Atom) -> Option<&Vec<Id>> {
    self.symbols.get(sym)
  }

  pub fn var(&self, id: &Id) -> Option<&Var> {
    self.vars.get(id)
  }

  pub fn var_by_ident(&self, ident: &view::Ident) -> Option<&Var> {
    self.var(&ident.inner.to_id())
  }

  pub fn is_global(&self, id: &Id) -> bool {
    self.var(id).is_none()
  }
}

#[derive(Debug)]
pub struct Var {
  path: Vec<ScopeKind>,
  kind: BindingKind,
}

impl Var {
  /// Empty path means root scope.
  #[allow(dead_code)]
  pub fn path(&self) -> &[ScopeKind] {
    &self.path
  }

  pub fn kind(&self) -> BindingKind {
    self.kind
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum BindingKind {
  Var,
  Const,
  Let,
  Function,
  Param,
  Class,
  CatchClause,

  /// This means that the binding comes from `ImportStarAsSpecifier`, like
  /// `import * as foo from "foo.ts";`
  /// `foo` effectively represents a namespace.
  NamespaceImport,

  /// Represents `ImportDefaultSpecifier` or `ImportNamedSpecifier`.
  /// e.g.
  ///   - import foo from "foo.ts";
  ///   - import { foo } from "foo.ts";
  ValueImport,

  Type,
}

impl BindingKind {
  pub fn is_import(&self) -> bool {
    matches!(
      *self,
      BindingKind::ValueImport | BindingKind::NamespaceImport
    )
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ScopeKind {
  // Module,
  Arrow,
  Function,
  Block,
  Loop,
  Class,
  Switch,
  With,
  Catch,
}

struct Analyzer<'a> {
  scope: &'a mut Scope,
  path: &'a mut Vec<ScopeKind>,
}

impl Analyzer<'_> {
  fn declare_id(&mut self, kind: BindingKind, i: Id) {
    self.scope.vars.insert(
      i.clone(),
      Var {
        kind,
        path: self.path.clone(),
      },
    );
    self.scope.symbols.entry(i.0.clone()).or_default().push(i);
  }

  fn declare(&mut self, kind: BindingKind, i: &Ident) {
    self.declare_id(kind, i.to_id());
  }

  fn declare_pat(&mut self, kind: BindingKind, pat: &Pat) {
    let ids: Vec<Id> = find_pat_ids(pat);

    for id in ids {
      self.declare_id(kind, id);
    }
  }

  fn visit_with_path<T>(&mut self, kind: ScopeKind, node: &T)
  where
    T: 'static + for<'any> VisitWith<Analyzer<'any>>,
  {
    self.path.push(kind);
    node.visit_with(self);
    self.path.pop();
  }

  fn with<F>(&mut self, kind: ScopeKind, op: F)
  where
    F: FnOnce(&mut Analyzer),
  {
    self.path.push(kind);
    op(self);
    self.path.pop();
  }
}

impl Visit for Analyzer<'_> {
  fn visit_arrow_expr(&mut self, n: &ArrowExpr) {
    self.with(ScopeKind::Arrow, |a| {
      // Parameters of `ArrowExpr` are of type `Vec<Pat>`, not `Vec<Param>`,
      // which means `visit_param` does _not_ handle parameters of `ArrowExpr`.
      // We need to handle them manually here.
      for param in &n.params {
        a.declare_pat(BindingKind::Param, param);
      }
      n.visit_children_with(a);
    });
  }

  /// Overriden not to add ScopeKind::Block
  fn visit_block_stmt_or_expr(&mut self, n: &BlockStmtOrExpr) {
    match n {
      BlockStmtOrExpr::BlockStmt(s) => s.stmts.visit_with(self),
      BlockStmtOrExpr::Expr(e) => e.visit_with(self),
    }
  }

  fn visit_var_decl(&mut self, n: &VarDecl) {
    n.decls.iter().for_each(|v| {
      v.init.visit_with(self);

      // If the class name and the variable name are the same like `let Foo = class Foo {}`,
      // this binding should be treated as `BindingKind::Class`.
      if let Some(expr) = &v.init {
        if let Expr::Class(ClassExpr {
          ident: Some(class_name),
          ..
        }) = &**expr
        {
          if let Pat::Ident(var_name) = &v.name {
            if var_name.id.sym == class_name.sym {
              self.declare(BindingKind::Class, class_name);
              return;
            }
          }
        }
      }

      self.declare_pat(
        match n.kind {
          VarDeclKind::Var => BindingKind::Var,
          VarDeclKind::Let => BindingKind::Let,
          VarDeclKind::Const => BindingKind::Const,
        },
        &v.name,
      );
    });
  }

  /// Overriden not to add ScopeKind::Block
  fn visit_function(&mut self, n: &Function) {
    n.decorators.visit_with(self);
    n.params.visit_with(self);

    // Don't add ScopeKind::Block
    if let Some(body) = &n.body {
      body.stmts.visit_with(self);
    }
  }

  fn visit_fn_decl(&mut self, n: &FnDecl) {
    self.declare(BindingKind::Function, &n.ident);

    self.visit_with_path(ScopeKind::Function, &n.function);
  }

  fn visit_fn_expr(&mut self, n: &FnExpr) {
    if let Some(ident) = &n.ident {
      self.declare(BindingKind::Function, ident);
    }

    self.visit_with_path(ScopeKind::Function, &n.function);
  }

  fn visit_class_decl(&mut self, n: &ClassDecl) {
    self.declare(BindingKind::Class, &n.ident);

    self.visit_with_path(ScopeKind::Class, &n.class);
  }

  fn visit_class_expr(&mut self, n: &ClassExpr) {
    if let Some(class_name) = n.ident.as_ref() {
      self.declare(BindingKind::Class, class_name);
    }

    self.visit_with_path(ScopeKind::Class, &n.class);
  }

  fn visit_block_stmt(&mut self, n: &BlockStmt) {
    self.visit_with_path(ScopeKind::Block, &n.stmts)
  }

  fn visit_catch_clause(&mut self, n: &CatchClause) {
    if let Some(pat) = &n.param {
      self.declare_pat(BindingKind::CatchClause, pat);
    }
    self.visit_with_path(ScopeKind::Catch, &n.body)
  }

  fn visit_param(&mut self, n: &Param) {
    self.declare_pat(BindingKind::Param, &n.pat);
  }

  fn visit_import_named_specifier(&mut self, n: &ImportNamedSpecifier) {
    self.declare(BindingKind::ValueImport, &n.local);
  }

  fn visit_import_default_specifier(&mut self, n: &ImportDefaultSpecifier) {
    self.declare(BindingKind::ValueImport, &n.local);
  }

  fn visit_import_star_as_specifier(&mut self, n: &ImportStarAsSpecifier) {
    self.declare(BindingKind::NamespaceImport, &n.local);
  }

  fn visit_with_stmt(&mut self, n: &WithStmt) {
    n.obj.visit_with(self);
    self.with(ScopeKind::With, |a| n.body.visit_children_with(a))
  }

  fn visit_for_stmt(&mut self, n: &ForStmt) {
    n.init.visit_with(self);
    n.update.visit_with(self);
    n.test.visit_with(self);

    self.visit_with_path(ScopeKind::Loop, &n.body);
  }

  fn visit_for_of_stmt(&mut self, n: &ForOfStmt) {
    n.left.visit_with(self);
    n.right.visit_with(self);

    self.visit_with_path(ScopeKind::Loop, &n.body);
  }

  fn visit_for_in_stmt(&mut self, n: &ForInStmt) {
    n.left.visit_with(self);
    n.right.visit_with(self);

    self.visit_with_path(ScopeKind::Loop, &n.body);
  }

  fn visit_do_while_stmt(&mut self, n: &DoWhileStmt) {
    n.test.visit_with(self);

    self.visit_with_path(ScopeKind::Loop, &n.body);
  }

  fn visit_while_stmt(&mut self, n: &WhileStmt) {
    n.test.visit_with(self);

    self.visit_with_path(ScopeKind::Loop, &n.body);
  }

  fn visit_switch_stmt(&mut self, n: &SwitchStmt) {
    n.discriminant.visit_with(self);

    self.visit_with_path(ScopeKind::Switch, &n.cases);
  }

  fn visit_ts_type_alias_decl(&mut self, n: &TsTypeAliasDecl) {
    self.declare(BindingKind::Type, &n.id);
  }

  fn visit_ts_interface_decl(&mut self, n: &TsInterfaceDecl) {
    self.declare(BindingKind::Type, &n.id);
  }
}

#[cfg(test)]
mod tests {
  use super::{BindingKind, Scope, ScopeKind, Var};
  use crate::parse_module;
  use crate::swc::ast::Id;
  use crate::MediaType;
  use crate::ModuleSpecifier;
  use crate::ParseParams;

  fn test_scope(source_code: &str, test: impl Fn(Scope)) {
    let parsed_source = parse_module(ParseParams {
      specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
      text: source_code.to_string().into(),
      media_type: MediaType::TypeScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();

    parsed_source.with_view(|view| {
      let scope = Scope::analyze(view);
      test(scope);
    });
  }

  fn id(scope: &Scope, s: &str) -> Id {
    let ids = scope.ids_with_symbol(&s.into());
    if ids.is_none() {
      panic!("No identifier named {}", s);
    }
    let ids = ids.unwrap();
    if ids.len() > 1 {
      panic!("Multiple identifers named {} found", s);
    }

    ids.first().unwrap().clone()
  }

  fn var<'a>(scope: &'a Scope, symbol: &str) -> &'a Var {
    scope.var(&id(scope, symbol)).unwrap()
  }

  #[test]
  fn scopes() {
    let source_code = r#"
  const a = "a";
  const unused = "unused";
  function asdf(b: number, c: string): number {
      console.log(a, b);
      {
        const c = 1;
        let d = 2;
      }
      return 1;
  }
  class Foo {
    #fizz = "fizz";
    bar() {
    }
  }
  try {
    // some code that might throw
    throw new Error("asdf");
  } catch (e) {
    const msg = "asdf " + e.message;
  }
  "#;
    test_scope(source_code, |scope| {
      assert_eq!(var(&scope, "a").kind(), BindingKind::Const);
      assert_eq!(var(&scope, "a").path(), &[]);

      assert_eq!(var(&scope, "b").kind(), BindingKind::Param);
      assert_eq!(scope.ids_with_symbol(&"c".into()).unwrap().len(), 2);
      assert_eq!(
        var(&scope, "d").path(),
        &[ScopeKind::Function, ScopeKind::Block]
      );

      assert_eq!(var(&scope, "Foo").kind(), BindingKind::Class);
      assert_eq!(var(&scope, "Foo").path(), &[]);

      assert_eq!(var(&scope, "e").kind(), BindingKind::CatchClause);
      assert_eq!(var(&scope, "e").path(), &[]);
    });
  }
}
