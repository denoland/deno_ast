// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::collections::HashMap;

use crate::swc::ast;
use crate::swc::atoms::Atom;
use crate::swc::atoms::JsWord;
use crate::swc::common::comments::CommentKind;
use crate::swc::common::comments::Comments;
use crate::swc::visit::Visit;
use crate::swc::visit::VisitWith;
use crate::SourcePos;
use crate::SourceRange;
use crate::SourceRangedForSpanned;

pub fn analyze_dependencies(
  module: &ast::Module,
  comments: &dyn Comments,
) -> Vec<DependencyDescriptor> {
  let mut v = DependencyCollector {
    comments,
    items: vec![],
    is_top_level: true,
  };
  module.visit_with(&mut v);
  v.items
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum DependencyKind {
  Import,
  ImportType,
  ImportEquals,
  Export,
  ExportType,
  ExportEquals,
  Require,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ImportAssertion {
  /// The value of this assertion could not be statically analyzed.
  Unknown,
  /// The value of this assertion is a statically analyzed string.
  Known(String),
}

#[derive(Clone, Default, Debug, Eq, PartialEq)]
pub enum ImportAttributes {
  /// There was no import assertions object literal.
  #[default]
  None,
  /// The set of assertion keys could not be statically analyzed.
  Unknown,
  /// The set of assertion keys is statically analyzed, though each respective
  /// value may or may not not be for dynamic imports.
  Known(HashMap<String, ImportAssertion>),
}

impl ImportAttributes {
  pub fn get(&self, key: &str) -> Option<&String> {
    match self {
      ImportAttributes::Known(map) => match map.get(key) {
        Some(ImportAssertion::Known(value)) => Some(value),
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
pub struct DependencyDescriptor {
  pub kind: DependencyKind,
  /// A flag indicating if the import is dynamic or not.
  pub is_dynamic: bool,
  /// Any leading comments associated with the dependency.  This is used for
  /// further processing of supported pragma that impact the dependency.
  pub leading_comments: Vec<DependencyComment>,
  /// The range of the import/export statement.
  pub range: SourceRange,
  /// The text specifier associated with the import/export statement.
  pub specifier: JsWord,
  /// The range of the specifier.
  pub specifier_range: SourceRange,
  /// Import assertions for this dependency.
  pub import_attributes: ImportAttributes,
}

struct DependencyCollector<'a> {
  comments: &'a dyn Comments,
  pub items: Vec<DependencyDescriptor>,
  // This field is used to determine if currently visited "require"
  // is top level and "static", or inside module body and "dynamic".
  is_top_level: bool,
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
    let import_attributes = parse_import_attributes(node.with.as_deref());
    self.items.push(DependencyDescriptor {
      kind,
      is_dynamic: false,
      leading_comments,
      range: node.range(),
      specifier,
      specifier_range: node.src.range(),
      import_attributes,
    });
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
      let import_attributes = parse_import_attributes(node.with.as_deref());
      self.items.push(DependencyDescriptor {
        kind,
        is_dynamic: false,
        leading_comments,
        range: node.range(),
        specifier,
        specifier_range: src.range(),
        import_attributes,
      });
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
    let import_attributes = parse_import_attributes(node.with.as_deref());
    self.items.push(DependencyDescriptor {
      kind,
      is_dynamic: false,
      leading_comments,
      range: node.range(),
      specifier,
      specifier_range: node.src.range(),
      import_attributes,
    });
  }

  fn visit_ts_import_type(&mut self, node: &ast::TsImportType) {
    let specifier = node.arg.value.clone();
    let leading_comments = self.get_leading_comments(node.start());
    self.items.push(DependencyDescriptor {
      kind: DependencyKind::ImportType,
      is_dynamic: false,
      leading_comments,
      range: node.range(),
      specifier,
      specifier_range: node.arg.range(),
      import_attributes: Default::default(),
    });
    node.visit_children_with(self);
  }

  fn visit_module_items(&mut self, items: &[ast::ModuleItem]) {
    swc_ecma_visit::visit_module_items(self, items);
  }

  fn visit_stmts(&mut self, items: &[ast::Stmt]) {
    self.is_top_level = false;
    swc_ecma_visit::visit_stmts(self, items);
    self.is_top_level = true;
  }

  fn visit_call_expr(&mut self, node: &ast::CallExpr) {
    use ast::{Callee, Expr, Ident, MemberProp};

    swc_ecma_visit::visit_call_expr(self, node);
    let kind = match &node.callee {
      Callee::Super(_) => return,
      Callee::Import(_) => DependencyKind::Import,
      Callee::Expr(expr) => match &**expr {
        Expr::Ident(Ident { sym: require, .. }) if &**require == "require" => {
          DependencyKind::Require
        }
        Expr::Member(member) => match (&*member.obj, &member.prop) {
          (
            Expr::Ident(Ident { sym: obj_sym, .. }),
            MemberProp::Ident(Ident { sym: prop_sym, .. }),
          ) if obj_sym == "require" && prop_sym == "resolve" => {
            DependencyKind::Require
          }
          _ => return,
        },
        _ => return,
      },
    };

    if let Some(arg) = node.args.first() {
      if let Expr::Lit(ast::Lit::Str(str_)) = &*arg.expr {
        // import() are always dynamic, even if at top level
        let is_dynamic = !self.is_top_level || kind == DependencyKind::Import;
        let dynamic_import_assertions = if kind == DependencyKind::Import {
          parse_dynamic_import_assertions(node.args.get(1))
        } else {
          Default::default()
        };
        let specifier = str_.value.clone();
        let leading_comments = self.get_leading_comments(node.start());
        self.items.push(DependencyDescriptor {
          kind,
          is_dynamic,
          leading_comments,
          range: node.range(),
          specifier,
          specifier_range: str_.range(),
          import_attributes: dynamic_import_assertions,
        });
      }
    }
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

      self.items.push(DependencyDescriptor {
        kind,
        is_dynamic: false,
        leading_comments,
        range: node.range(),
        specifier,
        specifier_range: expr.range(),
        import_attributes: Default::default(),
      });
    }
  }
}

/// Parses import assertions into a hashmap. According to proposal the values
/// can only be strings (https://github.com/tc39/proposal-import-assertions#should-more-than-just-strings-be-supported-as-attribute-values)
/// and thus non-string values are skipped.
fn parse_import_attributes(attrs: Option<&ast::ObjectLit>) -> ImportAttributes {
  let attrs = match attrs {
    Some(with) => with,
    None => return ImportAttributes::None,
  };
  let mut import_assertions = HashMap::new();
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
            import_assertions
              .insert(key, ImportAssertion::Known(str_.value.to_string()));
          }
        }
      }
    }
  }
  ImportAttributes::Known(import_assertions)
}

/// Parses import assertions from the second arg of a dynamic import.
fn parse_dynamic_import_assertions(
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

  let mut assertions_map = HashMap::new();
  let mut had_assert_key = false;

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
      had_assert_key = true;
      let assertions_lit = match &*key_value.value {
        ast::Expr::Object(assertions_lit) => assertions_lit,
        _ => return ImportAttributes::Unknown,
      };

      for prop in assertions_lit.props.iter() {
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
          assertions_map.insert(
            key,
            if let ast::Lit::Str(str_) = value_lit {
              ImportAssertion::Known(str_.value.to_string())
            } else {
              ImportAssertion::Unknown
            },
          );
        } else {
          assertions_map.insert(key, ImportAssertion::Unknown);
        }
      }
    }
  }

  if had_assert_key {
    ImportAttributes::Known(assertions_map)
  } else {
    ImportAttributes::None
  }
}

#[cfg(test)]
mod tests {
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
    (
      source.module().start(),
      analyze_dependencies(
        source.module(),
        &source.comments().as_swc_comments(),
      ),
    )
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
    assert_eq!(dependencies.len(), 13);
    assert_eq!(
      dependencies,
      vec![
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos, start_pos + 33),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 21, start_pos + 32),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::ImportType,
          is_dynamic: false,
          leading_comments: vec![DependencyComment {
            kind: CommentKind::Block,
            text: r#"* JSDoc "#.into(),
            range: SourceRange::new(start_pos + 34, start_pos + 46),
          }],
          range: SourceRange::new(start_pos + 47, start_pos + 85),
          specifier: JsWord::from("./foo.d.ts"),
          specifier_range: SourceRange::new(start_pos + 72, start_pos + 84),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Export,
          is_dynamic: false,
          leading_comments: vec![DependencyComment {
            kind: CommentKind::Line,
            text: r#"/ <reference foo="bar" />"#.into(),
            range: SourceRange::new(start_pos + 86, start_pos + 113),
          }],
          range: SourceRange::new(start_pos + 114, start_pos + 148),
          specifier: JsWord::from("./buzz.ts"),
          specifier_range: SourceRange::new(start_pos + 136, start_pos + 147),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::ExportType,
          is_dynamic: false,
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
        },
        DependencyDescriptor {
          kind: DependencyKind::Require,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 238, start_pos + 253),
          specifier: JsWord::from("path"),
          specifier_range: SourceRange::new(start_pos + 246, start_pos + 252),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 272, start_pos + 291),
          specifier: JsWord::from("./foo1.ts"),
          specifier_range: SourceRange::new(start_pos + 279, start_pos + 290),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 321, start_pos + 339),
          specifier: JsWord::from("./foo.ts"),
          specifier_range: SourceRange::new(start_pos + 328, start_pos + 338),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Require,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 391, start_pos + 414),
          specifier: JsWord::from("some_package"),
          specifier_range: SourceRange::new(start_pos + 399, start_pos + 413),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::ImportEquals,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 444, start_pos + 486),
          specifier: JsWord::from("some_package_foo"),
          specifier_range: SourceRange::new(start_pos + 466, start_pos + 484),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::ImportType,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 487, start_pos + 542),
          specifier: JsWord::from("some_package_foo_type"),
          specifier_range: SourceRange::new(start_pos + 517, start_pos + 540),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::ExportEquals,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 543, start_pos + 592),
          specifier: JsWord::from("some_package_bar"),
          specifier_range: SourceRange::new(start_pos + 572, start_pos + 590),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Require,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 606, start_pos + 645),
          specifier: JsWord::from("some_package_resolve"),
          specifier_range: SourceRange::new(start_pos + 622, start_pos + 644),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Require,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 670, start_pos + 713),
          specifier: JsWord::from("some_package_resolve_foo"),
          specifier_range: SourceRange::new(start_pos + 686, start_pos + 712),
          import_attributes: Default::default(),
        },
      ]
    );
  }

  #[test]
  fn test_import_assertions() {
    let source = r#"import * as bar from "./test.ts" assert { "type": "typescript" };
export * from "./test.ts" assert { "type": "typescript" };
export { bar } from "./test.json" assert { "type": "json" };
import foo from "./foo.json" assert { type: "json" };
const fizz = await import("./fizz.json", { "assert": { type: "json" } });
const buzz = await import("./buzz.json", { assert: { "type": "json" } });
const d1 = await import("./d1.json");
const d2 = await import("./d2.json", {});
const d3 = await import("./d3.json", bar);
const d4 = await import("./d4.json", { assert: {} });
const d5 = await import("./d5.json", { assert: bar });
const d6 = await import("./d6.json", { assert: {}, ...bar });
const d7 = await import("./d7.json", { assert: {}, ["assert"]: "bad" });
const d8 = await import("./d8.json", { assert: { type: bar } });
const d9 = await import("./d9.json", { assert: { type: "json", ...bar } });
const d10 = await import("./d10.json", { assert: { type: "json", ["type"]: "bad" } });
      "#;
    let (start_pos, dependencies) = helper("test.ts", source);
    let expected_assertions1 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAssertion::Known("typescript".to_string()),
      );
      map
    });
    let expected_assertions2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAssertion::Known("json".to_string()),
      );
      map
    });
    let dynamic_expected_assertions2 = ImportAttributes::Known({
      let mut map = HashMap::new();
      map.insert(
        "type".to_string(),
        ImportAssertion::Known("json".to_string()),
      );
      map
    });
    assert_eq!(dependencies.len(), 16);
    assert_eq!(
      dependencies,
      vec![
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos, start_pos + 65),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 21, start_pos + 32),
          import_attributes: expected_assertions1.clone(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Export,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 66, start_pos + 124),
          specifier: JsWord::from("./test.ts"),
          specifier_range: SourceRange::new(start_pos + 80, start_pos + 91),
          import_attributes: expected_assertions1,
        },
        DependencyDescriptor {
          kind: DependencyKind::Export,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 125, start_pos + 185),
          specifier: JsWord::from("./test.json"),
          specifier_range: SourceRange::new(start_pos + 145, start_pos + 158),
          import_attributes: expected_assertions2.clone(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 186, start_pos + 239),
          specifier: JsWord::from("./foo.json"),
          specifier_range: SourceRange::new(start_pos + 202, start_pos + 214),
          import_attributes: expected_assertions2,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 259, start_pos + 312),
          specifier: JsWord::from("./fizz.json"),
          specifier_range: SourceRange::new(start_pos + 266, start_pos + 279),
          import_attributes: dynamic_expected_assertions2.clone(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 333, start_pos + 386),
          specifier: JsWord::from("./buzz.json"),
          specifier_range: SourceRange::new(start_pos + 340, start_pos + 353),
          import_attributes: dynamic_expected_assertions2,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 405, start_pos + 424),
          specifier: JsWord::from("./d1.json"),
          specifier_range: SourceRange::new(start_pos + 412, start_pos + 423),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 443, start_pos + 466),
          specifier: JsWord::from("./d2.json"),
          specifier_range: SourceRange::new(start_pos + 450, start_pos + 461),
          import_attributes: Default::default(),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 485, start_pos + 509),
          specifier: JsWord::from("./d3.json"),
          specifier_range: SourceRange::new(start_pos + 492, start_pos + 503),
          import_attributes: ImportAttributes::Unknown,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 528, start_pos + 563),
          specifier: JsWord::from("./d4.json"),
          specifier_range: SourceRange::new(start_pos + 535, start_pos + 546),
          import_attributes: ImportAttributes::Known(HashMap::new()),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 582, start_pos + 618),
          specifier: JsWord::from("./d5.json"),
          specifier_range: SourceRange::new(start_pos + 589, start_pos + 600),
          import_attributes: ImportAttributes::Unknown,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 637, start_pos + 680),
          specifier: JsWord::from("./d6.json"),
          specifier_range: SourceRange::new(start_pos + 644, start_pos + 655),
          import_attributes: ImportAttributes::Unknown,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 699, start_pos + 753),
          specifier: JsWord::from("./d7.json"),
          specifier_range: SourceRange::new(start_pos + 706, start_pos + 717),
          import_attributes: ImportAttributes::Unknown,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 772, start_pos + 818),
          specifier: JsWord::from("./d8.json"),
          specifier_range: SourceRange::new(start_pos + 779, start_pos + 790),
          import_attributes: ImportAttributes::Known({
            let mut map = HashMap::new();
            map.insert("type".to_string(), ImportAssertion::Unknown);
            map
          }),
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 837, start_pos + 894),
          specifier: JsWord::from("./d9.json"),
          specifier_range: SourceRange::new(start_pos + 844, start_pos + 855),
          import_attributes: ImportAttributes::Unknown,
        },
        DependencyDescriptor {
          kind: DependencyKind::Import,
          is_dynamic: true,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 914, start_pos + 981),
          specifier: JsWord::from("./d10.json"),
          specifier_range: SourceRange::new(start_pos + 921, start_pos + 933),
          import_attributes: ImportAttributes::Unknown,
        },
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
        DependencyDescriptor {
          kind: DependencyKind::ImportType,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 46, start_pos + 174),
          specifier: JsWord::from("./a.d.ts"),
          specifier_range: SourceRange::new(start_pos + 53, start_pos + 63),
          import_attributes: ImportAttributes::None,
        },
        DependencyDescriptor {
          kind: DependencyKind::ImportType,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 93, start_pos + 129),
          specifier: JsWord::from("./b.d.ts"),
          specifier_range: SourceRange::new(start_pos + 100, start_pos + 110),
          import_attributes: ImportAttributes::None,
        },
        DependencyDescriptor {
          kind: DependencyKind::ImportType,
          is_dynamic: false,
          leading_comments: Vec::new(),
          range: SourceRange::new(start_pos + 135, start_pos + 173),
          specifier: JsWord::from("./c.d.ts"),
          specifier_range: SourceRange::new(start_pos + 142, start_pos + 152),
          import_attributes: ImportAttributes::None,
        }
      ]
    );
  }
}
