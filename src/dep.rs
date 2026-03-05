// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use oxc::ast::ast::BinaryExpression;
use oxc::ast::ast::CallExpression;
use oxc::ast::ast::Comment;
use oxc::ast::ast::Declaration;
use oxc::ast::ast::ExportAllDeclaration;
use oxc::ast::ast::ExportNamedDeclaration;
use oxc::ast::ast::Expression;
use oxc::ast::ast::ImportDeclaration;
use oxc::ast::ast::ImportExpression;
use oxc::ast::ast::ImportOrExportKind;
use oxc::ast::ast::ObjectExpression;
use oxc::ast::ast::ObjectPropertyKind;
use oxc::ast::ast::Program;
use oxc::ast::ast::PropertyKey;
use oxc::ast::ast::PropertyKind;
use oxc::ast::ast::Statement;
use oxc::ast::ast::TSImportEqualsDeclaration;
use oxc::ast::ast::TSImportType;
use oxc::ast::ast::TSModuleReference;
use oxc::ast::ast::TemplateLiteral;
use oxc::ast::ast::WithClause;
use oxc::ast_visit::walk;
use oxc::ast_visit::Visit;
use oxc::span::GetSpan;
use oxc::span::Span;
use serde::Deserialize;
use serde::Serialize;

use crate::ParsedSource;

impl ParsedSource<'_> {
  /// Analyzes the module for a list of its static and dynamic imports.
  ///
  /// Note: This will also include `require` calls as dynamic imports.
  pub fn analyze_dependencies(&self) -> Vec<DependencyDescriptor> {
    analyze_program_dependencies(&self.program, self.text())
  }
}

pub fn analyze_program_dependencies(
  program: &Program<'_>,
  source_text: &str,
) -> Vec<DependencyDescriptor> {
  let mut v = DependencyCollector {
    comments: &program.comments,
    source_text,
    items: vec![],
  };
  v.visit_program(program);
  v.items
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum StaticDependencyKind {
  Import,
  ImportType,
  ImportEquals,
  Export,
  ExportType,
  ExportEquals,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
#[serde(untagged)]
pub enum ImportAttribute {
  /// The value of this attribute could not be statically analyzed.
  Unknown,
  /// The value of this attribute is a statically analyzed string.
  Known(String),
}

#[derive(Clone, Default, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ImportAttributes {
  /// There was no import attributes object literal.
  #[default]
  None,
  /// The set of attribute keys could not be statically analyzed.
  Unknown,
  /// The set of attribute keys is statically analyzed, though each respective
  /// value may or may not not be for dynamic imports.
  Known(HashMap<String, ImportAttribute>),
}

impl ImportAttributes {
  pub fn is_none(&self) -> bool {
    matches!(self, ImportAttributes::None)
  }

  pub fn get(&self, key: &str) -> Option<&String> {
    match self {
      ImportAttributes::Known(map) => match map.get(key) {
        Some(ImportAttribute::Known(value)) => Some(value),
        _ => None,
      },
      _ => None,
    }
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DependencyComment {
  pub kind: DependencyCommentKind,
  pub span: Span,
  pub text: String,
}

/// Equivalent to swc's CommentKind but mapping OXC's three comment kinds.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DependencyCommentKind {
  Line,
  Block,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DependencyDescriptor {
  Static(StaticDependencyDescriptor),
  Dynamic(DynamicDependencyDescriptor),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StaticDependencyDescriptor {
  pub kind: StaticDependencyKind,
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub span: Span,
  /// The text specifier associated with the import/export statement.
  pub specifier: String,
  /// The range of the specifier.
  pub specifier_span: Span,
  /// Import attributes for this dependency.
  pub import_attributes: ImportAttributes,
}

impl From<StaticDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: StaticDependencyDescriptor) -> Self {
    DependencyDescriptor::Static(descriptor)
  }
}

#[derive(
  Default, Clone, Copy, Debug, Eq, PartialEq, Serialize, Deserialize,
)]
#[serde(rename_all = "camelCase")]
pub enum DynamicDependencyKind {
  #[default]
  Import,
  Require,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DynamicDependencyDescriptor {
  /// Kind of dynamic dependency.
  pub kind: DynamicDependencyKind,
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub span: Span,
  /// The argument associated with the dynamic import
  pub argument: DynamicArgument,
  /// The range of the specifier.
  pub argument_span: Span,
  /// Import attributes for this dependency.
  pub import_attributes: ImportAttributes,
}

impl From<DynamicDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: DynamicDependencyDescriptor) -> Self {
    DependencyDescriptor::Dynamic(descriptor)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DynamicArgument {
  String(String),
  Template(Vec<DynamicTemplatePart>),
  /// An expression that could not be analyzed.
  Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DynamicTemplatePart {
  String(String),
  /// An expression that could not be analyzed.
  Expr,
}

struct DependencyCollector<'a, 'b> {
  comments: &'b oxc::allocator::Vec<'a, Comment>,
  source_text: &'b str,
  pub items: Vec<DependencyDescriptor>,
}

impl<'a, 'b> DependencyCollector<'a, 'b> {
  fn get_leading_comments(&self, span_start: u32) -> Vec<DependencyComment> {
    self
      .comments
      .iter()
      .filter(|c| c.is_leading() && c.attached_to == span_start)
      .map(|c| {
        let content_span = c.content_span();
        let text =
          self.source_text[content_span.start as usize..content_span.end as usize].to_string();
        DependencyComment {
          kind: if c.is_line() {
            DependencyCommentKind::Line
          } else {
            DependencyCommentKind::Block
          },
          span: c.span,
          text,
        }
      })
      .collect()
  }

  fn is_require(&self, callee: &Expression<'a>) -> bool {
    matches!(callee, Expression::Identifier(ident) if ident.name == "require")
  }

  fn handle_import_expression(&mut self, node: &ImportExpression<'a>) {
    let (argument, argument_span) = match &node.source {
      Expression::StringLiteral(s) => {
        (DynamicArgument::String(s.value.to_string()), s.span)
      }
      Expression::TemplateLiteral(tpl) => {
        let (arg, span) = self.analyze_template_literal(tpl);
        (arg, span)
      }
      Expression::BinaryExpression(bin) => {
        let arg = self.analyze_binary_expr(bin);
        (arg, node.source.span())
      }
      other => (DynamicArgument::Expr, other.span()),
    };

    let import_attributes = match &node.options {
      Some(expr) => parse_dynamic_import_attributes_from_expr(expr),
      None => ImportAttributes::None,
    };

    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      DynamicDependencyDescriptor {
        kind: DynamicDependencyKind::Import,
        leading_comments,
        span: node.span,
        argument,
        argument_span,
        import_attributes,
      }
      .into(),
    );
  }

  fn analyze_template_literal(
    &self,
    tpl: &TemplateLiteral<'a>,
  ) -> (DynamicArgument, Span) {
    let span = tpl.span;
    if tpl.quasis.len() == 1 && tpl.expressions.is_empty() {
      let cooked = tpl.quasis[0]
        .value
        .cooked
        .as_ref()
        .map(|s| s.to_string())
        .unwrap_or_default();
      (DynamicArgument::String(cooked), span)
    } else {
      let mut parts =
        Vec::with_capacity(tpl.quasis.len() + tpl.expressions.len());
      for i in 0..tpl.quasis.len() {
        if let Some(cooked) = &tpl.quasis[i].value.cooked {
          if !cooked.is_empty() {
            parts.push(DynamicTemplatePart::String(cooked.to_string()));
          }
        }
        if tpl.expressions.get(i).is_some() {
          parts.push(DynamicTemplatePart::Expr);
        }
      }
      (DynamicArgument::Template(parts), span)
    }
  }

  fn analyze_binary_expr(
    &self,
    bin: &BinaryExpression<'a>,
  ) -> DynamicArgument {
    use oxc::syntax::operator::BinaryOperator;

    let mut parts = Vec::with_capacity(2);

    fn visit_bin<'a>(
      parts: &mut Vec<DynamicTemplatePart>,
      bin: &BinaryExpression<'a>,
    ) -> Result<(), ()> {
      if bin.operator != BinaryOperator::Addition {
        return Err(());
      }

      match &bin.left {
        Expression::BinaryExpression(left) => {
          visit_bin(parts, left)?;
        }
        Expression::StringLiteral(s) => {
          parts.push(DynamicTemplatePart::String(s.value.to_string()));
        }
        _ => {
          if parts.is_empty() {
            return Err(());
          }
          parts.push(DynamicTemplatePart::Expr);
        }
      };

      if let Expression::StringLiteral(s) = &bin.right {
        parts.push(DynamicTemplatePart::String(s.value.to_string()));
      } else {
        parts.push(DynamicTemplatePart::Expr);
      }

      Ok(())
    }

    if visit_bin(&mut parts, bin).is_ok() {
      DynamicArgument::Template(parts)
    } else {
      DynamicArgument::Expr
    }
  }
}

impl<'a> Visit<'a> for DependencyCollector<'a, '_> {
  fn visit_import_declaration(&mut self, node: &ImportDeclaration<'a>) {
    let specifier = node.source.value.to_string();
    let leading_comments = self.get_leading_comments(node.span.start);
    let kind = if node.import_kind == ImportOrExportKind::Type {
      StaticDependencyKind::ImportType
    } else {
      StaticDependencyKind::Import
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        specifier,
        specifier_span: node.source.span,
        import_attributes: parse_import_attributes(
          node.with_clause.as_deref(),
        ),
      }
      .into(),
    );
  }

  fn visit_export_named_declaration(
    &mut self,
    node: &ExportNamedDeclaration<'a>,
  ) {
    // Handle `export import foo = require(...)` - OXC wraps this as
    // ExportNamedDeclaration with declaration = TSImportEqualsDeclaration
    if let Some(Declaration::TSImportEqualsDeclaration(import_eq)) =
      &node.declaration
    {
      self.handle_export_import_equals(node.span, import_eq);
      return;
    }

    if let Some(src) = &node.source {
      let specifier = src.value.to_string();
      let leading_comments = self.get_leading_comments(node.span.start);
      let kind = if node.export_kind == ImportOrExportKind::Type {
        StaticDependencyKind::ExportType
      } else {
        StaticDependencyKind::Export
      };
      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          span: node.span,
          specifier,
          specifier_span: src.span,
          import_attributes: parse_import_attributes(
            node.with_clause.as_deref(),
          ),
        }
        .into(),
      );
    }

    // Walk children to find TSImportType nodes in type annotations
    walk::walk_export_named_declaration(self, node);
  }

  fn visit_export_all_declaration(&mut self, node: &ExportAllDeclaration<'a>) {
    let specifier = node.source.value.to_string();
    let leading_comments = self.get_leading_comments(node.span.start);
    let kind = if node.export_kind == ImportOrExportKind::Type {
      StaticDependencyKind::ExportType
    } else {
      StaticDependencyKind::Export
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        span: node.span,
        specifier,
        specifier_span: node.source.span,
        import_attributes: parse_import_attributes(
          node.with_clause.as_deref(),
        ),
      }
      .into(),
    );
  }

  fn visit_ts_import_type(&mut self, node: &TSImportType<'a>) {
    let specifier = node.source.value.to_string();
    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      StaticDependencyDescriptor {
        kind: StaticDependencyKind::ImportType,
        leading_comments,
        span: node.span,
        specifier,
        specifier_span: node.source.span,
        import_attributes: node
          .options
          .as_ref()
          .map(|opts| parse_import_attributes_from_options_object(opts))
          .unwrap_or_default(),
      }
      .into(),
    );
    walk::walk_ts_import_type(self, node);
  }

  fn visit_statements(&mut self, stmts: &oxc::allocator::Vec<'a, Statement<'a>>) {
    walk::walk_statements(self, stmts);
  }

  fn visit_call_expression(&mut self, node: &CallExpression<'a>) {
    walk::walk_call_expression(self, node);

    if !self.is_require(&node.callee) {
      return;
    }
    let Some(arg) = node.arguments.first() else {
      return;
    };

    let arg_expr = match arg {
      oxc::ast::ast::Argument::SpreadElement(_) => return,
      _ => arg.as_expression().unwrap(),
    };

    let (argument, argument_span) = match arg_expr {
      Expression::StringLiteral(s) => {
        (DynamicArgument::String(s.value.to_string()), s.span)
      }
      Expression::TemplateLiteral(tpl) => self.analyze_template_literal(tpl),
      Expression::BinaryExpression(bin) => {
        (self.analyze_binary_expr(bin), arg_expr.span())
      }
      other => (DynamicArgument::Expr, other.span()),
    };
    let leading_comments = self.get_leading_comments(node.span.start);
    self.items.push(
      DynamicDependencyDescriptor {
        kind: DynamicDependencyKind::Require,
        leading_comments,
        span: node.span,
        argument,
        argument_span,
        import_attributes: Default::default(),
      }
      .into(),
    );
  }

  fn visit_import_expression(&mut self, node: &ImportExpression<'a>) {
    walk::walk_import_expression(self, node);
    self.handle_import_expression(node);
  }

  fn visit_ts_import_equals_declaration(
    &mut self,
    node: &TSImportEqualsDeclaration<'a>,
  ) {
    if let TSModuleReference::ExternalModuleReference(module) =
      &node.module_reference
    {
      let leading_comments = self.get_leading_comments(node.span.start);
      let expr = &module.expression;
      let specifier = expr.value.to_string();

      let kind = if node.import_kind == ImportOrExportKind::Type {
        StaticDependencyKind::ImportType
      } else {
        StaticDependencyKind::ImportEquals
      };

      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          span: node.span,
          specifier,
          specifier_span: expr.span,
          import_attributes: Default::default(),
        }
        .into(),
      );
    }
  }
}

impl DependencyCollector<'_, '_> {
  fn handle_export_import_equals(
    &mut self,
    export_span: Span,
    node: &TSImportEqualsDeclaration<'_>,
  ) {
    if let TSModuleReference::ExternalModuleReference(module) =
      &node.module_reference
    {
      let leading_comments = self.get_leading_comments(export_span.start);
      let expr = &module.expression;
      let specifier = expr.value.to_string();

      let kind = if node.import_kind == ImportOrExportKind::Type {
        StaticDependencyKind::ImportType
      } else {
        StaticDependencyKind::ExportEquals
      };

      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          span: export_span,
          specifier,
          specifier_span: expr.span,
          import_attributes: Default::default(),
        }
        .into(),
      );
    }
  }
}

/// Parses import attributes from WithClause.
fn parse_import_attributes(
  maybe_with: Option<&WithClause<'_>>,
) -> ImportAttributes {
  let Some(with_clause) = maybe_with else {
    return ImportAttributes::None;
  };
  let mut import_attributes = HashMap::new();
  for attr in &with_clause.with_entries {
    let key = match &attr.key {
      oxc::ast::ast::ImportAttributeKey::Identifier(ident) => {
        ident.name.to_string()
      }
      oxc::ast::ast::ImportAttributeKey::StringLiteral(s) => {
        s.value.to_string()
      }
    };
    import_attributes
      .insert(key, ImportAttribute::Known(attr.value.value.to_string()));
  }
  ImportAttributes::Known(import_attributes)
}

/// Parses import attributes from the second arg of a dynamic import.
fn parse_dynamic_import_attributes_from_expr(
  arg: &Expression<'_>,
) -> ImportAttributes {
  let object_lit = match arg {
    Expression::ObjectExpression(object_lit) => object_lit,
    _ => return ImportAttributes::Unknown,
  };
  let mut attributes_map = HashMap::new();
  let mut had_attributes_key = false;
  let mut had_with_key = false;

  for prop in &object_lit.properties {
    let prop = match prop {
      ObjectPropertyKind::ObjectProperty(prop) => prop,
      _ => return ImportAttributes::Unknown,
    };
    if prop.kind != PropertyKind::Init || prop.computed {
      return ImportAttributes::Unknown;
    }
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if key == "with" || key == "assert" && !had_with_key {
      had_attributes_key = true;
      had_with_key = key == "with";
      let attributes_lit = match &prop.value {
        Expression::ObjectExpression(lit) => lit,
        _ => return ImportAttributes::Unknown,
      };
      match parse_import_attributes_from_object_lit(attributes_lit) {
        ImportAttributes::Known(hash_map) => {
          attributes_map = hash_map;
        }
        value => return value,
      }
    }
  }

  if had_attributes_key {
    ImportAttributes::Known(attributes_map)
  } else {
    ImportAttributes::None
  }
}

/// Parses import attributes from an ObjectExpression used in
/// `import("./foo", { with: { type: "json" } })` or
/// `import("./foo", { with: { "resolution-mode": "import" } })`.
fn parse_import_attributes_from_options_object(
  opts: &ObjectExpression<'_>,
) -> ImportAttributes {
  // Look for the `with` key in the options object
  for prop in &opts.properties {
    let prop = match prop {
      ObjectPropertyKind::ObjectProperty(prop) => prop,
      _ => return ImportAttributes::Unknown,
    };
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => continue,
    };
    if key == "with" {
      let attributes_lit = match &prop.value {
        Expression::ObjectExpression(lit) => lit,
        _ => return ImportAttributes::Unknown,
      };
      return parse_import_attributes_from_object_lit(attributes_lit);
    }
  }
  ImportAttributes::None
}

fn parse_import_attributes_from_object_lit(
  attributes_lit: &ObjectExpression<'_>,
) -> ImportAttributes {
  let mut attributes_map =
    HashMap::with_capacity(attributes_lit.properties.len());

  for prop in &attributes_lit.properties {
    let prop = match prop {
      ObjectPropertyKind::ObjectProperty(prop) => prop,
      _ => return ImportAttributes::Unknown,
    };
    if prop.computed {
      return ImportAttributes::Unknown;
    }
    let key = match &prop.key {
      PropertyKey::StringLiteral(s) => s.value.to_string(),
      PropertyKey::StaticIdentifier(ident) => ident.name.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if let Expression::StringLiteral(str_) = &prop.value {
      attributes_map
        .insert(key, ImportAttribute::Known(str_.value.to_string()));
    } else {
      attributes_map.insert(key, ImportAttribute::Unknown);
    }
  }
  ImportAttributes::Known(attributes_map)
}

#[cfg(test)]
mod tests {
  use oxc::allocator::Allocator;
  use oxc::span::Span;
  use pretty_assertions::assert_eq;

  use crate::ModuleSpecifier;

  use super::*;

  fn helper(specifier: &str, source: &str) -> Vec<DependencyDescriptor> {
    let allocator = Allocator::default();
    let parsed = crate::parse_module(
      &allocator,
      crate::ParseParams {
        specifier: ModuleSpecifier::parse(specifier).unwrap(),
        text: source.into(),
        media_type: crate::MediaType::Tsx,
        capture_tokens: false,
        scope_analysis: false,
        maybe_source_type: None,
      },
    )
    .unwrap();
    parsed.analyze_dependencies()
  }

  /// Helper to create a DependencyComment. OXC comment spans include
  /// the delimiters (// or /* */), so we store those spans.
  fn line_comment(span: Span, text: &str) -> DependencyComment {
    DependencyComment {
      kind: DependencyCommentKind::Line,
      span,
      text: text.to_string(),
    }
  }

  fn block_comment(span: Span, text: &str) -> DependencyComment {
    DependencyComment {
      kind: DependencyCommentKind::Block,
      span,
      text: text.to_string(),
    }
  }

  #[test]
  fn test_parsed_module_get_dependencies() {
    let source = r#"import * as bar from "./test.ts";
/** JSDoc */
import type { Foo } from "./foo.d.ts";
/// <reference foo="bar" />
export * as Buzz from "./buzz.ts";
// @some-pragma
/**
 * Foo
 */
export type { Fizz } from "./fizz.d.ts";
const { join } = require("path");
// dynamic
await import("./foo1.ts");
try {
    const foo = await import("./foo.ts");
} catch (e) {
    // pass
}
try {
    const foo = require("some_package");
} catch (e) {
    // pass
}
import foo2 = require("some_package_foo");
import type FooType = require('some_package_foo_type');
export import bar2 = require("some_package_bar");
const foo3 = require.resolve("some_package_resolve");
try {
    const foo4 = require.resolve("some_package_resolve_foo");
} catch (e) {
    // pass
}
      "#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(0, 33),
          specifier: "./test.ts".to_string(),
          specifier_span: Span::new(21, 32),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: vec![block_comment(
            Span::new(34, 46),
            "* JSDoc ",
          )],
          span: Span::new(47, 85),
          specifier: "./foo.d.ts".to_string(),
          specifier_span: Span::new(72, 84),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: vec![line_comment(
            Span::new(86, 113),
            "/ <reference foo=\"bar\" />",
          )],
          span: Span::new(114, 148),
          specifier: "./buzz.ts".to_string(),
          specifier_span: Span::new(136, 147),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ExportType,
          leading_comments: vec![
            line_comment(Span::new(149, 164), " @some-pragma"),
            block_comment(Span::new(165, 179), "*\n * Foo\n "),
          ],
          span: Span::new(180, 220),
          specifier: "./fizz.d.ts".to_string(),
          specifier_span: Span::new(206, 219),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          span: Span::new(238, 253),
          argument: DynamicArgument::String("path".to_string()),
          argument_span: Span::new(246, 252),
          import_attributes: Default::default(),
          kind: DynamicDependencyKind::Require,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(272, 291),
          argument: DynamicArgument::String("./foo1.ts".to_string()),
          argument_span: Span::new(279, 290),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(321, 339),
          argument: DynamicArgument::String("./foo.ts".to_string()),
          argument_span: Span::new(328, 338),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Require,
          leading_comments: Vec::new(),
          span: Span::new(391, 414),
          argument: DynamicArgument::String("some_package".to_string()),
          argument_span: Span::new(399, 413),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportEquals,
          leading_comments: Vec::new(),
          span: Span::new(444, 486),
          specifier: "some_package_foo".to_string(),
          specifier_span: Span::new(466, 484),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: Span::new(487, 542),
          specifier: "some_package_foo_type".to_string(),
          specifier_span: Span::new(517, 540),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ExportEquals,
          leading_comments: Vec::new(),
          span: Span::new(543, 592),
          specifier: "some_package_bar".to_string(),
          specifier_span: Span::new(572, 590),
          import_attributes: Default::default(),
        }
        .into(),
      ]
    );
  }

  #[test]
  fn test_import_attributes() {
    let source = r#"import * as bar from "./test.ts" with { "type": "typescript" };
export * from "./test.ts" with { "type": "typescript" };
export { bar } from "./test.json" with { "type": "json" };
import foo from "./foo.json" with { type: "json" };
const fizz = await import("./fizz.json", { "with": { type: "json" } });
const buzz = await import("./buzz.json", { with: { "type": "json" } });
const d1 = await import("./d1.json");
const d2 = await import("./d2.json", {});
const d3 = await import("./d3.json", bar);
const d4 = await import("./d4.json", { with: {} });
const d5 = await import("./d5.json", { with: bar });
const d6 = await import("./d6.json", { with: {}, ...bar });
const d7 = await import("./d7.json", { with: {}, ["assert"]: "bad" });
const d8 = await import("./d8.json", { with: { type: bar } });
const d9 = await import("./d9.json", { with: { type: "json", ...bar } });
const d10 = await import("./d10.json", { with: { type: "json", ["type"]: "bad" } });
      "#;
    let dependencies = helper("file:///test.ts", source);
    let expected_attributes1 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("typescript".to_string()),
      );
      map
    });
    let expected_attributes2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("json".to_string()),
      );
      map
    });
    let dynamic_expected_attributes2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAttribute::Known("json".to_string()),
      );
      map
    });
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(0, 63),
          specifier: "./test.ts".to_string(),
          specifier_span: Span::new(21, 32),
          import_attributes: expected_attributes1.clone(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: Vec::new(),
          span: Span::new(64, 120),
          specifier: "./test.ts".to_string(),
          specifier_span: Span::new(78, 89),
          import_attributes: expected_attributes1,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Export,
          leading_comments: Vec::new(),
          span: Span::new(121, 179),
          specifier: "./test.json".to_string(),
          specifier_span: Span::new(141, 154),
          import_attributes: expected_attributes2.clone(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(180, 231),
          specifier: "./foo.json".to_string(),
          specifier_span: Span::new(196, 208),
          import_attributes: expected_attributes2,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(251, 302),
          argument: DynamicArgument::String("./fizz.json".to_string()),
          argument_span: Span::new(258, 271),
          import_attributes: dynamic_expected_attributes2.clone(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(323, 374),
          argument: DynamicArgument::String("./buzz.json".to_string()),
          argument_span: Span::new(330, 343),
          import_attributes: dynamic_expected_attributes2,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(393, 412),
          argument: DynamicArgument::String("./d1.json".to_string()),
          argument_span: Span::new(400, 411),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(431, 454),
          argument: DynamicArgument::String("./d2.json".to_string()),
          argument_span: Span::new(438, 449),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(473, 497),
          argument: DynamicArgument::String("./d3.json".to_string()),
          argument_span: Span::new(480, 491),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(516, 549),
          argument: DynamicArgument::String("./d4.json".to_string()),
          argument_span: Span::new(523, 534),
          import_attributes: ImportAttributes::Known(HashMap::new()),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(568, 602),
          argument: DynamicArgument::String("./d5.json".to_string()),
          argument_span: Span::new(575, 586),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(621, 662),
          argument: DynamicArgument::String("./d6.json".to_string()),
          argument_span: Span::new(628, 639),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(681, 733),
          argument: DynamicArgument::String("./d7.json".to_string()),
          argument_span: Span::new(688, 699),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(752, 796),
          argument: DynamicArgument::String("./d8.json".to_string()),
          argument_span: Span::new(759, 770),
          import_attributes: ImportAttributes::Known({
            let mut map = HashMap::new();
            map.insert("type".to_string(), ImportAttribute::Unknown);
            map
          }),
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(815, 870),
          argument: DynamicArgument::String("./d9.json".to_string()),
          argument_span: Span::new(822, 833),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(890, 955),
          argument: DynamicArgument::String("./d10.json".to_string()),
          argument_span: Span::new(897, 909),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
      ]
    );
  }

  #[test]
  fn test_dynamic_imports() {
    let source = r#"const d1 = await import(`./d1.json`);
const d2 = await import(`${value}`);
const d3 = await import(`./test/${value}`);
const d4 = await import(`${value}/test`);
const d5 = await import(`${value}${value2}`);
const d6 = await import(`${value}/test/${value2}`);
const d7 = await import(`./${value}/test/${value2}/`);
const d8 = await import("./foo/" + value);
const d9 = await import("./foo/" + value + ".ts");
const d10 = await import(value + ".ts");
const d11 = await import("./foo/" - value);
const d12 = await import(expr);
"#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(17, 36),
          argument: DynamicArgument::String("./d1.json".to_string()),
          argument_span: Span::new(24, 35),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(55, 73),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr
          ]),
          argument_span: Span::new(62, 72),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(92, 117),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./test/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: Span::new(99, 116),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(136, 159),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test".to_string()),
          ]),
          argument_span: Span::new(143, 158),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(178, 205),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::Expr,
          ]),
          argument_span: Span::new(185, 204),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(224, 257),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: Span::new(231, 256),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(276, 312),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/test/".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String("/".to_string()),
          ]),
          argument_span: Span::new(283, 311),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(331, 355),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./foo/".to_string()),
            DynamicTemplatePart::Expr,
          ]),
          argument_span: Span::new(338, 354),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(374, 406),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String("./foo/".to_string()),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(".ts".to_string()),
          ]),
          argument_span: Span::new(381, 405),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(426, 447),
          argument: DynamicArgument::Expr,
          argument_span: Span::new(433, 446),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(467, 491),
          argument: DynamicArgument::Expr,
          argument_span: Span::new(474, 490),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          kind: DynamicDependencyKind::Import,
          leading_comments: Vec::new(),
          span: Span::new(511, 523),
          argument: DynamicArgument::Expr,
          argument_span: Span::new(518, 522),
          import_attributes: ImportAttributes::None,
        }
        .into(),
      ]
    );
  }

  #[test]
  fn ts_import_object_lit_property() {
    let source = r#"
export declare const SomeValue: typeof Core & import("./a.d.ts").Constructor<{
    paginate: import("./b.d.ts").PaginateInterface;
} & import("./c.d.ts", { with: { "resolution-mode": "import" } }).RestEndpointMethods>;
"#;
    let dependencies = helper("file:///test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: Span::new(47, 218),
          specifier: "./a.d.ts".to_string(),
          specifier_span: Span::new(54, 64),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: Span::new(94, 130),
          specifier: "./b.d.ts".to_string(),
          specifier_span: Span::new(101, 111),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: StaticDependencyKind::ImportType,
          leading_comments: Vec::new(),
          span: Span::new(136, 217),
          specifier: "./c.d.ts".to_string(),
          specifier_span: Span::new(143, 153),
          import_attributes: ImportAttributes::Known(HashMap::from([(
            "resolution-mode".to_string(),
            ImportAttribute::Known("import".to_string())
          )])),
        }
        .into()
      ]
    );
  }
}
