// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use serde::Deserialize;
use serde::Serialize;

use crate::swc::ast;
use crate::swc::ast::Callee;
use crate::swc::ast::Expr;
use crate::swc::atoms::Atom;
use crate::swc::common::comments::CommentKind;
use crate::swc::common::comments::Comments;
use crate::swc::visit::Visit;
use crate::swc::visit::VisitWith;
use crate::ParsedSource;
use crate::SourcePos;
use crate::SourceRange;
use crate::SourceRangedForSpanned;

impl ParsedSource {
  /// Analyzes the module for a list of its imports and exports.
  pub fn analyze_dependencies(&self) -> Vec<DependencyDescriptor> {
    let module = match self.program_ref() {
      ast::Program::Module(module) => module,
      ast::Program::Script(_) => return vec![],
    };

    let mut v = DependencyCollector {
      comments: &self.comments().as_swc_comments(),
      items: vec![],
    };
    module.visit_with(&mut v);
    v.items
  }
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum DependencyKind {
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
  pub kind: CommentKind,
  pub range: SourceRange,
  pub text: Atom,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DependencyDescriptor {
  Static(StaticDependencyDescriptor),
  Dynamic(DynamicDependencyDescriptor),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StaticDependencyDescriptor {
  pub kind: DependencyKind,
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub range: SourceRange,
  /// The text specifier associated with the import/export statement.
  pub specifier: Atom,
  /// The range of the specifier.
  pub specifier_range: SourceRange,
  /// Import attributes for this dependency.
  pub import_attributes: ImportAttributes,
}

impl From<StaticDependencyDescriptor> for DependencyDescriptor {
  fn from(descriptor: StaticDependencyDescriptor) -> Self {
    DependencyDescriptor::Static(descriptor)
  }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct DynamicDependencyDescriptor {
  /// Any leading comments associated with the dependency. This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub range: SourceRange,
  /// The argument associated with the dynamic import
  pub argument: DynamicArgument,
  /// The range of the specifier.
  pub argument_range: SourceRange,
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
  String(Atom),
  Template(Vec<DynamicTemplatePart>),
  /// An expression that could not be analyzed.
  Expr,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DynamicTemplatePart {
  String(Atom),
  /// An expression that could not be analyzed.
  Expr,
}

struct DependencyCollector<'a> {
  comments: &'a dyn Comments,
  pub items: Vec<DependencyDescriptor>,
}

impl<'a> DependencyCollector<'a> {
  fn get_leading_comments(&self, start: SourcePos) -> Vec<DependencyComment> {
    self
      .comments
      .get_leading(start.as_byte_pos())
      .unwrap_or_default()
      .into_iter()
      .map(|c| DependencyComment {
        kind: c.kind,
        range: c.range(),
        text: c.text,
      })
      .collect()
  }
}

impl<'a> Visit for DependencyCollector<'a> {
  fn visit_import_decl(&mut self, node: &ast::ImportDecl) {
    let specifier = node.src.value.clone();
    let leading_comments = self.get_leading_comments(node.start());
    let kind = if node.type_only {
      DependencyKind::ImportType
    } else {
      DependencyKind::Import
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        range: node.range(),
        specifier,
        specifier_range: node.src.range(),
        import_attributes: parse_import_attributes(node.with.as_deref()),
      }
      .into(),
    );
  }

  fn visit_named_export(&mut self, node: &ast::NamedExport) {
    if let Some(src) = &node.src {
      let specifier = src.value.clone();
      let leading_comments = self.get_leading_comments(node.start());
      let kind = if node.type_only {
        DependencyKind::ExportType
      } else {
        DependencyKind::Export
      };
      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          range: node.range(),
          specifier,
          specifier_range: src.range(),
          import_attributes: parse_import_attributes(node.with.as_deref()),
        }
        .into(),
      );
    }
  }

  fn visit_export_all(&mut self, node: &ast::ExportAll) {
    let specifier = node.src.value.clone();
    let leading_comments = self.get_leading_comments(node.start());
    let kind = if node.type_only {
      DependencyKind::ExportType
    } else {
      DependencyKind::Export
    };
    self.items.push(
      StaticDependencyDescriptor {
        kind,
        leading_comments,
        range: node.range(),
        specifier,
        specifier_range: node.src.range(),
        import_attributes: parse_import_attributes(node.with.as_deref()),
      }
      .into(),
    );
  }

  fn visit_ts_import_type(&mut self, node: &ast::TsImportType) {
    let specifier = node.arg.value.clone();
    let leading_comments = self.get_leading_comments(node.start());
    self.items.push(
      StaticDependencyDescriptor {
        kind: DependencyKind::ImportType,
        leading_comments,
        range: node.range(),
        specifier,
        specifier_range: node.arg.range(),
        import_attributes: Default::default(),
      }
      .into(),
    );
    node.visit_children_with(self);
  }

  fn visit_module_items(&mut self, items: &[ast::ModuleItem]) {
    swc_ecma_visit::visit_module_items(self, items);
  }

  fn visit_stmts(&mut self, items: &[ast::Stmt]) {
    swc_ecma_visit::visit_stmts(self, items);
  }

  fn visit_call_expr(&mut self, node: &ast::CallExpr) {
    swc_ecma_visit::visit_call_expr(self, node);

    if !matches!(&node.callee, Callee::Import(_)) {
      return;
    }
    let Some(arg) = node.args.first() else {
      return;
    };

    let argument = match &*arg.expr {
      Expr::Lit(ast::Lit::Str(specifier)) => {
        DynamicArgument::String(specifier.value.clone())
      }
      Expr::Tpl(tpl) => {
        if tpl.quasis.len() == 1 && tpl.exprs.is_empty() {
          DynamicArgument::String(tpl.quasis[0].raw.clone())
        } else {
          let mut parts =
            Vec::with_capacity(tpl.quasis.len() + tpl.exprs.len());
          for i in 0..tpl.quasis.len() {
            if tpl.quasis[i].raw.len() > 0 {
              parts
                .push(DynamicTemplatePart::String(tpl.quasis[i].raw.clone()));
            }
            if tpl.exprs.get(i).is_some() {
              parts.push(DynamicTemplatePart::Expr);
            }
          }
          DynamicArgument::Template(parts)
        }
      }
      _ => DynamicArgument::Expr,
    };
    let dynamic_import_attributes =
      parse_dynamic_import_attributes(node.args.get(1));
    let leading_comments = self.get_leading_comments(node.start());
    self.items.push(
      DynamicDependencyDescriptor {
        leading_comments,
        range: node.range(),
        argument,
        argument_range: arg.range(),
        import_attributes: dynamic_import_attributes,
      }
      .into(),
    );
  }

  fn visit_ts_import_equals_decl(&mut self, node: &ast::TsImportEqualsDecl) {
    use ast::TsModuleRef;

    if let TsModuleRef::TsExternalModuleRef(module) = &node.module_ref {
      let leading_comments = self.get_leading_comments(node.start());
      let expr = &module.expr;
      let specifier = expr.value.clone();

      let kind = if node.is_type_only {
        DependencyKind::ImportType
      } else if node.is_export {
        DependencyKind::ExportEquals
      } else {
        DependencyKind::ImportEquals
      };

      self.items.push(
        StaticDependencyDescriptor {
          kind,
          leading_comments,
          range: node.range(),
          specifier,
          specifier_range: expr.range(),
          import_attributes: Default::default(),
        }
        .into(),
      );
    }
  }
}

/// Parses import attributes into a hashmap. According to proposal the values
/// can only be strings (https://github.com/tc39/proposal-import-attributes#should-more-than-just-strings-be-supported-as-attribute-values)
/// and thus non-string values are skipped.
fn parse_import_attributes(
  maybe_attrs: Option<&ast::ObjectLit>,
) -> ImportAttributes {
  let Some(attrs) = maybe_attrs else {
    return ImportAttributes::None;
  };
  let mut import_attributes = HashMap::new();
  for prop in attrs.props.iter() {
    if let ast::PropOrSpread::Prop(prop) = prop {
      if let ast::Prop::KeyValue(key_value) = &**prop {
        let maybe_key = match &key_value.key {
          ast::PropName::Str(key) => Some(key.value.to_string()),
          ast::PropName::Ident(ident) => Some(ident.sym.to_string()),
          _ => None,
        };

        if let Some(key) = maybe_key {
          if let ast::Expr::Lit(ast::Lit::Str(str_)) = &*key_value.value {
            import_attributes
              .insert(key, ImportAttribute::Known(str_.value.to_string()));
          }
        }
      }
    }
  }
  ImportAttributes::Known(import_attributes)
}

/// Parses import attributes from the second arg of a dynamic import.
fn parse_dynamic_import_attributes(
  arg: Option<&ast::ExprOrSpread>,
) -> ImportAttributes {
  let arg = match arg {
    Some(arg) => arg,
    None => return ImportAttributes::None,
  };

  if arg.spread.is_some() {
    return ImportAttributes::Unknown;
  }

  let object_lit = match &*arg.expr {
    ast::Expr::Object(object_lit) => object_lit,
    _ => return ImportAttributes::Unknown,
  };

  let mut attributes_map = HashMap::new();
  let mut had_attributes_key = false;

  for prop in object_lit.props.iter() {
    let prop = match prop {
      ast::PropOrSpread::Prop(prop) => prop,
      _ => return ImportAttributes::Unknown,
    };
    let key_value = match &**prop {
      ast::Prop::KeyValue(key_value) => key_value,
      _ => return ImportAttributes::Unknown,
    };
    let key = match &key_value.key {
      ast::PropName::Str(key) => key.value.to_string(),
      ast::PropName::Ident(ident) => ident.sym.to_string(),
      _ => return ImportAttributes::Unknown,
    };
    if key == "assert" || key == "with" {
      had_attributes_key = true;
      let attributes_lit = match &*key_value.value {
        ast::Expr::Object(lit) => lit,
        _ => return ImportAttributes::Unknown,
      };

      for prop in attributes_lit.props.iter() {
        let prop = match prop {
          ast::PropOrSpread::Prop(prop) => prop,
          _ => return ImportAttributes::Unknown,
        };
        let key_value = match &**prop {
          ast::Prop::KeyValue(key_value) => key_value,
          _ => return ImportAttributes::Unknown,
        };
        let key = match &key_value.key {
          ast::PropName::Str(key) => key.value.to_string(),
          ast::PropName::Ident(ident) => ident.sym.to_string(),
          _ => return ImportAttributes::Unknown,
        };
        if let ast::Expr::Lit(value_lit) = &*key_value.value {
          attributes_map.insert(
            key,
            if let ast::Lit::Str(str_) = value_lit {
              ImportAttribute::Known(str_.value.to_string())
            } else {
              ImportAttribute::Unknown
            },
          );
        } else {
          attributes_map.insert(key, ImportAttribute::Unknown);
        }
      }
    }
  }

  if had_attributes_key {
    ImportAttributes::Known(attributes_map)
  } else {
    ImportAttributes::None
  }
}

#[cfg(test)]
mod tests {
  use crate::swc::atoms::JsWord;
  use crate::swc::common::comments::CommentKind;
  use crate::SourcePos;
  use crate::SourceRange;
  use crate::SourceRangedForSpanned;

  use pretty_assertions::assert_eq;

  use super::*;

  fn helper(
    file_name: &str,
    source: &str,
  ) -> (SourcePos, Vec<DependencyDescriptor>) {
    let source = crate::parse_module(crate::ParseParams {
      specifier: file_name.to_string(),
      text_info: crate::SourceTextInfo::from_string(source.to_string()),
      media_type: crate::MediaType::Tsx,
      capture_tokens: false,
      scope_analysis: false,
      maybe_syntax: None,
    })
    .unwrap();
    (source.module().start(), source.analyze_dependencies())
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
    let (start_pos, dependencies) = helper("test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: DependencyKind::Import,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos, start_pos + 33),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 21, start_pos + 32),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportType,
          leading_comments: vec![DependencyComment {
            kind: CommentKind::Block,
            text: r#"* JSDoc "#.into(),
            range: SourceRange::new(start_pos + 34, start_pos + 46),
          }],
          range: SourceRange::new(start_pos + 47, start_pos + 85),
          specifier: JsWord::from("./foo.d.ts"),
          specifier_range: SourceRange::new(start_pos + 72, start_pos + 84),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::Export,
          leading_comments: vec![DependencyComment {
            kind: CommentKind::Line,
            text: r#"/ <reference foo="bar" />"#.into(),
            range: SourceRange::new(start_pos + 86, start_pos + 113),
          }],
          range: SourceRange::new(start_pos + 114, start_pos + 148),
          specifier: JsWord::from("./buzz.ts"),
          specifier_range: SourceRange::new(start_pos + 136, start_pos + 147),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ExportType,
          leading_comments: vec![
            DependencyComment {
              kind: CommentKind::Line,
              text: r#" @some-pragma"#.into(),
              range: SourceRange::new(start_pos + 149, start_pos + 164),
            },
            DependencyComment {
              kind: CommentKind::Block,
              text: "*\n * Foo\n ".into(),
              range: SourceRange::new(start_pos + 165, start_pos + 179),
            }
          ],
          range: SourceRange::new(start_pos + 180, start_pos + 220),
          specifier: JsWord::from("./fizz.d.ts"),
          specifier_range: SourceRange::new(start_pos + 206, start_pos + 219),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 272, start_pos + 291),
          argument: DynamicArgument::String(JsWord::from("./foo1.ts")),
          argument_range: SourceRange::new(start_pos + 279, start_pos + 290),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 321, start_pos + 339),
          argument: DynamicArgument::String(JsWord::from("./foo.ts")),
          argument_range: SourceRange::new(start_pos + 328, start_pos + 338),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportEquals,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 444, start_pos + 486),
          specifier: JsWord::from("some_package_foo"),
          specifier_range: SourceRange::new(start_pos + 466, start_pos + 484),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportType,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 487, start_pos + 542),
          specifier: JsWord::from("some_package_foo_type"),
          specifier_range: SourceRange::new(start_pos + 517, start_pos + 540),
          import_attributes: Default::default(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ExportEquals,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 543, start_pos + 592),
          specifier: JsWord::from("some_package_bar"),
          specifier_range: SourceRange::new(start_pos + 572, start_pos + 590),
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
    let (start_pos, dependencies) = helper("test.ts", source);
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
          kind: DependencyKind::Import,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos, start_pos + 63),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 21, start_pos + 32),
          import_attributes: expected_attributes1.clone(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::Export,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 64, start_pos + 120),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 78, start_pos + 89),
          import_attributes: expected_attributes1,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::Export,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 121, start_pos + 179),
          specifier: JsWord::from("./test.json"),
          specifier_range: SourceRange::new(start_pos + 141, start_pos + 154),
          import_attributes: expected_attributes2.clone(),
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::Import,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 180, start_pos + 231),
          specifier: JsWord::from("./foo.json"),
          specifier_range: SourceRange::new(start_pos + 196, start_pos + 208),
          import_attributes: expected_attributes2,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 251, start_pos + 302),
          argument: DynamicArgument::String(JsWord::from("./fizz.json")),
          argument_range: SourceRange::new(start_pos + 258, start_pos + 271),
          import_attributes: dynamic_expected_attributes2.clone(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 323, start_pos + 374),
          argument: DynamicArgument::String(JsWord::from("./buzz.json")),
          argument_range: SourceRange::new(start_pos + 330, start_pos + 343),
          import_attributes: dynamic_expected_attributes2,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 393, start_pos + 412),
          argument: DynamicArgument::String(JsWord::from("./d1.json")),
          argument_range: SourceRange::new(start_pos + 400, start_pos + 411),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 431, start_pos + 454),
          argument: DynamicArgument::String(JsWord::from("./d2.json")),
          argument_range: SourceRange::new(start_pos + 438, start_pos + 449),
          import_attributes: Default::default(),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 473, start_pos + 497),
          argument: DynamicArgument::String(JsWord::from("./d3.json")),
          argument_range: SourceRange::new(start_pos + 480, start_pos + 491),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 516, start_pos + 549),
          argument: DynamicArgument::String(JsWord::from("./d4.json")),
          argument_range: SourceRange::new(start_pos + 523, start_pos + 534),
          import_attributes: ImportAttributes::Known(HashMap::new()),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 568, start_pos + 602),
          argument: DynamicArgument::String(JsWord::from("./d5.json")),
          argument_range: SourceRange::new(start_pos + 575, start_pos + 586),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 621, start_pos + 662),
          argument: DynamicArgument::String(JsWord::from("./d6.json")),
          argument_range: SourceRange::new(start_pos + 628, start_pos + 639),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 681, start_pos + 733),
          argument: DynamicArgument::String(JsWord::from("./d7.json")),
          argument_range: SourceRange::new(start_pos + 688, start_pos + 699),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 752, start_pos + 796),
          argument: DynamicArgument::String(JsWord::from("./d8.json")),
          argument_range: SourceRange::new(start_pos + 759, start_pos + 770),
          import_attributes: ImportAttributes::Known({
            let mut map = HashMap::new();
            map.insert("type".to_string(), ImportAttribute::Unknown);
            map
          }),
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 815, start_pos + 870),
          argument: DynamicArgument::String(JsWord::from("./d9.json")),
          argument_range: SourceRange::new(start_pos + 822, start_pos + 833),
          import_attributes: ImportAttributes::Unknown,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 890, start_pos + 955),
          argument: DynamicArgument::String(JsWord::from("./d10.json")),
          argument_range: SourceRange::new(start_pos + 897, start_pos + 909),
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
const d8 = await import(expr);
"#;
    let (start_pos, dependencies) = helper("test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 17, start_pos + 36),
          argument: DynamicArgument::String(JsWord::from("./d1.json")),
          argument_range: SourceRange::new(start_pos + 24, start_pos + 35),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 55, start_pos + 73),
          argument: DynamicArgument::Template(vec![DynamicTemplatePart::Expr]),
          argument_range: SourceRange::new(start_pos + 62, start_pos + 72),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 92, start_pos + 117),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String(JsWord::from("./test/")),
            DynamicTemplatePart::Expr,
          ]),
          argument_range: SourceRange::new(start_pos + 99, start_pos + 116),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 136, start_pos + 159),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(JsWord::from("/test")),
          ]),
          argument_range: SourceRange::new(start_pos + 143, start_pos + 158),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 178, start_pos + 205),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::Expr,
          ]),
          argument_range: SourceRange::new(start_pos + 185, start_pos + 204),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 224, start_pos + 257),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(JsWord::from("/test/")),
            DynamicTemplatePart::Expr,
          ]),
          argument_range: SourceRange::new(start_pos + 231, start_pos + 256),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 276, start_pos + 312),
          argument: DynamicArgument::Template(vec![
            DynamicTemplatePart::String(JsWord::from("./")),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(JsWord::from("/test/")),
            DynamicTemplatePart::Expr,
            DynamicTemplatePart::String(JsWord::from("/")),
          ]),
          argument_range: SourceRange::new(start_pos + 283, start_pos + 311),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        DynamicDependencyDescriptor {
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 331, start_pos + 343),
          argument: DynamicArgument::Expr,
          argument_range: SourceRange::new(start_pos + 338, start_pos + 342),
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
} & import("./c.d.ts").RestEndpointMethods>;
"#;
    let (start_pos, dependencies) = helper("test.ts", source);
    assert_eq!(
      dependencies,
      vec![
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportType,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 46, start_pos + 174),
          specifier: JsWord::from("./a.d.ts"),
          specifier_range: SourceRange::new(start_pos + 53, start_pos + 63),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportType,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 93, start_pos + 129),
          specifier: JsWord::from("./b.d.ts"),
          specifier_range: SourceRange::new(start_pos + 100, start_pos + 110),
          import_attributes: ImportAttributes::None,
        }
        .into(),
        StaticDependencyDescriptor {
          kind: DependencyKind::ImportType,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 135, start_pos + 173),
          specifier: JsWord::from("./c.d.ts"),
          specifier_range: SourceRange::new(start_pos + 142, start_pos + 152),
          import_attributes: ImportAttributes::None,
        }
        .into()
      ]
    );
  }
}
