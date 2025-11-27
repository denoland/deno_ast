// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;

use crate::ParsedSource;
use crate::ProgramRef;
use crate::swc::ast::ExportSpecifier;
use crate::swc::ast::ModuleDecl;
use crate::swc::ast::ModuleItem;
use crate::swc::atoms::Atom;
use crate::swc::utils::find_pat_ids;

#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ModuleExportsAndReExports {
  pub exports: Vec<String>,
  pub reexports: Vec<String>,
}

impl ParsedSource {
  /// Analyzes the ES runtime exports for require ESM.
  ///
  /// This is used during CJS export analysis when a CJS module
  /// re-exports an ESM module and the original CJS module needs
  /// to know the exports of the ESM module so it can create its
  /// wrapper ESM module.
  pub fn analyze_es_runtime_exports(&self) -> ModuleExportsAndReExports {
    let mut result = ModuleExportsAndReExports::default();
    if let ProgramRef::Module(n) = self.program_ref() {
      for m in &n.body {
        match m {
          ModuleItem::ModuleDecl(m) => match m {
            ModuleDecl::Import(_) => {}
            ModuleDecl::ExportAll(n) => {
              result
                .reexports
                .push(n.src.value.to_string_lossy().into_owned());
            }
            ModuleDecl::ExportDecl(d) => {
              match &d.decl {
                swc_ecma_ast::Decl::Class(d) => {
                  result.exports.push(d.ident.sym.to_string());
                }
                swc_ecma_ast::Decl::Fn(d) => {
                  result.exports.push(d.ident.sym.to_string());
                }
                swc_ecma_ast::Decl::Var(d) => {
                  for d in &d.decls {
                    for id in find_pat_ids::<_, Atom>(&d.name) {
                      result.exports.push(id.to_string());
                    }
                  }
                }
                swc_ecma_ast::Decl::TsEnum(d) => {
                  result.exports.push(d.id.sym.to_string())
                }
                swc_ecma_ast::Decl::TsModule(ts_module_decl) => {
                  match &ts_module_decl.id {
                    swc_ecma_ast::TsModuleName::Ident(ident) => {
                      result.exports.push(ident.sym.to_string())
                    }
                    swc_ecma_ast::TsModuleName::Str(_) => {
                      // ignore
                    }
                  }
                }
                swc_ecma_ast::Decl::Using(d) => {
                  for d in &d.decls {
                    for id in find_pat_ids::<_, Atom>(&d.name) {
                      result.exports.push(id.to_string());
                    }
                  }
                }
                swc_ecma_ast::Decl::TsInterface(_)
                | swc_ecma_ast::Decl::TsTypeAlias(_) => {
                  // ignore types
                }
              }
            }
            ModuleDecl::ExportNamed(n) => {
              for s in &n.specifiers {
                match s {
                  ExportSpecifier::Namespace(s) => {
                    result.exports.push(s.name.atom().to_string());
                  }
                  ExportSpecifier::Default(_) => {
                    result.exports.push("default".to_string());
                  }
                  ExportSpecifier::Named(n) => {
                    result.exports.push(
                      n.exported
                        .as_ref()
                        .map(|e| e.atom().to_string())
                        .unwrap_or_else(|| n.orig.atom().to_string()),
                    );
                  }
                }
              }
            }
            ModuleDecl::ExportDefaultExpr(_)
            | ModuleDecl::ExportDefaultDecl(_) => {
              result.exports.push("default".to_string());
            }
            ModuleDecl::TsImportEquals(_)
            | ModuleDecl::TsExportAssignment(_) => {
              // ignore because it's cjs
            }
            ModuleDecl::TsNamespaceExport(_) => {
              // ignore `export as namespace x;` as it's type only
            }
          },
          ModuleItem::Stmt(_) => {}
        }
      }
    }
    result
  }
}

#[cfg(test)]
mod test {
  use std::cell::RefCell;

  use deno_media_type::MediaType;

  use crate::ModuleSpecifier;
  use crate::ParseParams;
  use crate::parse_module;

  use super::ModuleExportsAndReExports;

  struct Tester {
    analysis: RefCell<ModuleExportsAndReExports>,
  }

  impl Tester {
    pub fn assert_exports(&self, values: Vec<&str>) {
      let mut analysis = self.analysis.borrow_mut();
      assert_eq!(analysis.exports, values);
      analysis.exports.clear();
    }

    pub fn assert_reexports(&self, values: Vec<&str>) {
      let mut analysis = self.analysis.borrow_mut();
      assert_eq!(analysis.reexports, values);
      analysis.reexports.clear();
    }

    pub fn assert_empty(&self) {
      let analysis = self.analysis.borrow();
      if !analysis.exports.is_empty() {
        panic!("Had exports: {}", analysis.exports.join(", "))
      }
      if !analysis.reexports.is_empty() {
        panic!("Had reexports: {}", analysis.reexports.join(", "))
      }
    }
  }

  impl Drop for Tester {
    fn drop(&mut self) {
      // ensures that all values have been asserted for
      if !std::thread::panicking() {
        self.assert_empty();
      }
    }
  }

  fn parse(source: &str) -> Tester {
    let parsed_source = parse_module(ParseParams {
      specifier: ModuleSpecifier::parse("file:///example.ts").unwrap(),
      text: source.into(),
      media_type: MediaType::TypeScript,
      capture_tokens: true,
      scope_analysis: false,
      maybe_syntax: None,
    })
    .unwrap();
    let analysis = parsed_source.analyze_es_runtime_exports();
    Tester {
      analysis: RefCell::new(analysis),
    }
  }

  #[test]
  fn runtime_exports_basic() {
    let tester = parse(
      "
export class A {}
export enum B {}
export module C.Test {}
export namespace C2.Test {}
export function d() {}
export const e = 1, f = 2;
export { g, h1 as h, other as 'testing-this' };
export * as y from './other.js';
export { z } from './other.js';
class Ignored1 {}
enum Ignored2 {}
module Ignored3 {}
namespace Ignored4 {}
function Ignored5() {}
const Ignored6 = 1;
",
    );
    tester.assert_exports(vec![
      "A",
      "B",
      "C",
      "C2",
      "d",
      "e",
      "f",
      "g",
      "h",
      "testing-this",
      "y",
      "z",
    ]);
  }

  #[test]
  fn runtime_exports_default_expr() {
    let tester = parse("export default 5;");
    tester.assert_exports(vec!["default"]);
  }

  #[test]
  fn runtime_exports_default_decl() {
    let tester = parse("export default class MyClass {}");
    tester.assert_exports(vec!["default"]);
  }

  #[test]
  fn runtime_exports_default_named_export() {
    let tester = parse("export { a as default }");
    tester.assert_exports(vec!["default"]);
  }

  #[test]
  fn runtime_re_export() {
    let tester = parse("export * from './other.js';");
    tester.assert_reexports(vec!["./other.js"]);
  }
}
