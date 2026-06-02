// Copyright 2020-2022 the Deno authors. All rights reserved. MIT license.

use oxc::ast::ast::*;
use oxc::ast_visit::Visit;
use oxc::ast_visit::walk;
use oxc::syntax::scope::ScopeFlags;
use std::collections::HashMap;

/// Uniquely identifies a binding declaration.
/// The first element is the name, the second is a disambiguation index
/// for cases where multiple declarations share the same name.
pub type Id = (String, usize);

#[derive(Debug)]
pub struct Scope {
  vars: HashMap<Id, Var>,
  symbols: HashMap<String, Vec<Id>>,
  next_id: usize,
}

impl Scope {
  pub fn analyze(program: &Program<'_>) -> Self {
    let mut scope = Self {
      vars: Default::default(),
      symbols: Default::default(),
      next_id: 0,
    };
    let mut path = vec![];
    let mut analyzer = Analyzer {
      scope: &mut scope,
      path: &mut path,
    };
    analyzer.visit_program(program);
    scope
  }

  /// Get all declarations with a symbol.
  pub fn ids_with_symbol(&self, sym: &str) -> Option<&Vec<Id>> {
    self.symbols.get(sym)
  }

  pub fn var(&self, id: &Id) -> Option<&Var> {
    self.vars.get(id)
  }

  pub fn var_by_name(&self, name: &str) -> Option<&Var> {
    let ids = self.symbols.get(name)?;
    let id = ids.first()?;
    self.vars.get(id)
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
  NamespaceImport,
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

  /// Convert from OXC's `SymbolFlags` to `BindingKind`.
  pub fn from_symbol_flags(flags: oxc::syntax::symbol::SymbolFlags) -> Self {
    use oxc::syntax::symbol::SymbolFlags;
    if flags.contains(SymbolFlags::Function) {
      BindingKind::Function
    } else if flags.contains(SymbolFlags::Class) {
      BindingKind::Class
    } else if flags.contains(SymbolFlags::Import) {
      BindingKind::ValueImport
    } else if flags.contains(SymbolFlags::CatchVariable) {
      BindingKind::CatchClause
    } else if flags.contains(SymbolFlags::ConstVariable) {
      BindingKind::Const
    } else if flags.contains(SymbolFlags::BlockScopedVariable) {
      BindingKind::Let
    } else if flags.intersects(SymbolFlags::TypeAlias | SymbolFlags::Interface)
    {
      BindingKind::Type
    } else {
      BindingKind::Var
    }
  }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum ScopeKind {
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
  fn declare(&mut self, kind: BindingKind, name: &str) {
    let id_index = self.scope.next_id;
    self.scope.next_id += 1;
    let id = (name.to_string(), id_index);
    self.scope.vars.insert(
      id.clone(),
      Var {
        kind,
        path: self.path.clone(),
      },
    );
    self
      .scope
      .symbols
      .entry(name.to_string())
      .or_default()
      .push(id);
  }

  fn declare_binding_pattern(
    &mut self,
    kind: BindingKind,
    pattern: &BindingPattern<'_>,
  ) {
    match pattern {
      BindingPattern::BindingIdentifier(ident) => {
        self.declare(kind, ident.name.as_str());
      }
      BindingPattern::ObjectPattern(obj) => {
        for prop in &obj.properties {
          self.declare_binding_pattern(kind, &prop.value);
        }
        if let Some(rest) = &obj.rest {
          self.declare_binding_pattern(kind, &rest.argument);
        }
      }
      BindingPattern::ArrayPattern(arr) => {
        for elem in arr.elements.iter().flatten() {
          self.declare_binding_pattern(kind, elem);
        }
        if let Some(rest) = &arr.rest {
          self.declare_binding_pattern(kind, &rest.argument);
        }
      }
      BindingPattern::AssignmentPattern(assign) => {
        self.declare_binding_pattern(kind, &assign.left);
      }
    }
  }

  fn with_scope<F>(&mut self, kind: ScopeKind, op: F)
  where
    F: FnOnce(&mut Analyzer),
  {
    self.path.push(kind);
    op(self);
    self.path.pop();
  }
}

impl<'a> Visit<'a> for Analyzer<'_> {
  fn visit_variable_declaration(&mut self, decl: &VariableDeclaration<'a>) {
    for declarator in &decl.declarations {
      if let Some(init) = &declarator.init {
        self.visit_expression(init);
      }

      let kind = match decl.kind {
        VariableDeclarationKind::Var => BindingKind::Var,
        VariableDeclarationKind::Let => BindingKind::Let,
        VariableDeclarationKind::Const => BindingKind::Const,
        VariableDeclarationKind::Using
        | VariableDeclarationKind::AwaitUsing => BindingKind::Const,
      };

      // Check for `let Foo = class Foo {}` pattern
      if let Some(init) = &declarator.init
        && let Expression::ClassExpression(class_expr) = init
        && let Some(class_id) = &class_expr.id
        && let Some(binding_id) = declarator.id.get_binding_identifier()
        && binding_id.name == class_id.name
      {
        self.declare(BindingKind::Class, class_id.name.as_str());
        continue;
      }

      self.declare_binding_pattern(kind, &declarator.id);
    }
  }

  fn visit_function(&mut self, func: &Function<'a>, _flags: ScopeFlags) {
    if let Some(id) = &func.id {
      self.declare(BindingKind::Function, id.name.as_str());
    }
    self.with_scope(ScopeKind::Function, |a| {
      for param in &func.params.items {
        a.declare_binding_pattern(BindingKind::Param, &param.pattern);
      }
      if let Some(rest) = &func.params.rest {
        a.declare_binding_pattern(BindingKind::Param, &rest.rest.argument);
      }
      if let Some(body) = &func.body {
        for stmt in &body.statements {
          a.visit_statement(stmt);
        }
      }
    });
  }

  fn visit_arrow_function_expression(
    &mut self,
    expr: &ArrowFunctionExpression<'a>,
  ) {
    self.with_scope(ScopeKind::Arrow, |a| {
      for param in &expr.params.items {
        a.declare_binding_pattern(BindingKind::Param, &param.pattern);
      }
      if let Some(rest) = &expr.params.rest {
        a.declare_binding_pattern(BindingKind::Param, &rest.rest.argument);
      }
      for stmt in &expr.body.statements {
        a.visit_statement(stmt);
      }
    });
  }

  fn visit_class(&mut self, class: &Class<'a>) {
    if let Some(id) = &class.id {
      self.declare(BindingKind::Class, id.name.as_str());
    }
    self.with_scope(ScopeKind::Class, |a| {
      walk::walk_class(a, class);
    });
  }

  fn visit_block_statement(&mut self, block: &BlockStatement<'a>) {
    self.with_scope(ScopeKind::Block, |a| {
      for stmt in &block.body {
        a.visit_statement(stmt);
      }
    });
  }

  fn visit_catch_clause(&mut self, clause: &CatchClause<'a>) {
    if let Some(param) = &clause.param {
      self.declare_binding_pattern(BindingKind::CatchClause, &param.pattern);
    }
    self.with_scope(ScopeKind::Catch, |a| {
      for stmt in &clause.body.body {
        a.visit_statement(stmt);
      }
    });
  }

  fn visit_import_declaration(&mut self, decl: &ImportDeclaration<'a>) {
    if let Some(specifiers) = &decl.specifiers {
      for spec in specifiers {
        match spec {
          ImportDeclarationSpecifier::ImportDefaultSpecifier(s) => {
            self.declare(BindingKind::ValueImport, s.local.name.as_str());
          }
          ImportDeclarationSpecifier::ImportSpecifier(s) => {
            self.declare(BindingKind::ValueImport, s.local.name.as_str());
          }
          ImportDeclarationSpecifier::ImportNamespaceSpecifier(s) => {
            self.declare(BindingKind::NamespaceImport, s.local.name.as_str());
          }
        }
      }
    }
  }

  fn visit_for_statement(&mut self, stmt: &ForStatement<'a>) {
    if let Some(init) = &stmt.init {
      self.visit_for_statement_init(init);
    }
    if let Some(update) = &stmt.update {
      self.visit_expression(update);
    }
    if let Some(test) = &stmt.test {
      self.visit_expression(test);
    }
    self.with_scope(ScopeKind::Loop, |a| {
      a.visit_statement(&stmt.body);
    });
  }

  fn visit_for_in_statement(&mut self, stmt: &ForInStatement<'a>) {
    self.visit_for_statement_left(&stmt.left);
    self.visit_expression(&stmt.right);
    self.with_scope(ScopeKind::Loop, |a| {
      a.visit_statement(&stmt.body);
    });
  }

  fn visit_for_of_statement(&mut self, stmt: &ForOfStatement<'a>) {
    self.visit_for_statement_left(&stmt.left);
    self.visit_expression(&stmt.right);
    self.with_scope(ScopeKind::Loop, |a| {
      a.visit_statement(&stmt.body);
    });
  }

  fn visit_while_statement(&mut self, stmt: &WhileStatement<'a>) {
    self.visit_expression(&stmt.test);
    self.with_scope(ScopeKind::Loop, |a| {
      a.visit_statement(&stmt.body);
    });
  }

  fn visit_do_while_statement(&mut self, stmt: &DoWhileStatement<'a>) {
    self.visit_expression(&stmt.test);
    self.with_scope(ScopeKind::Loop, |a| {
      a.visit_statement(&stmt.body);
    });
  }

  fn visit_switch_statement(&mut self, stmt: &SwitchStatement<'a>) {
    self.visit_expression(&stmt.discriminant);
    self.with_scope(ScopeKind::Switch, |a| {
      for case in &stmt.cases {
        a.visit_switch_case(case);
      }
    });
  }

  fn visit_ts_type_alias_declaration(
    &mut self,
    decl: &TSTypeAliasDeclaration<'a>,
  ) {
    self.declare(BindingKind::Type, decl.id.name.as_str());
    // Type parameters are in scope within the type alias body
    if let Some(type_params) = &decl.type_parameters {
      self.with_scope(ScopeKind::Block, |a| {
        for param in &type_params.params {
          a.declare(BindingKind::Type, param.name.name.as_str());
        }
        a.visit_ts_type(&decl.type_annotation);
      });
    }
  }

  fn visit_ts_interface_declaration(
    &mut self,
    decl: &TSInterfaceDeclaration<'a>,
  ) {
    self.declare(BindingKind::Type, decl.id.name.as_str());
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::MediaType;
  use crate::ModuleSpecifier;
  use crate::ParseParams;
  use crate::parse_module;
  use oxc::allocator::Allocator;

  fn test_scope(source_code: &str, test: impl Fn(Scope)) {
    let allocator = Allocator::default();
    let parsed_source = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: source_code.to_string().into(),
        media_type: MediaType::TypeScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: true,
      },
    )
    .unwrap();

    let scope = Scope::analyze(parsed_source.program());
    test(scope);
  }

  fn id(scope: &Scope, s: &str) -> Id {
    let ids = scope.ids_with_symbol(s);
    if ids.is_none() {
      panic!("No identifier named {}", s);
    }
    let ids = ids.unwrap();
    if ids.len() > 1 {
      panic!("Multiple identifiers named {} found", s);
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
      assert_eq!(scope.ids_with_symbol("c").unwrap().len(), 2);
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
