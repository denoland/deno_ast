// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use serde::Deserialize;
use serde::Serialize;

use oxc::ast::ast::*;

use crate::ParsedSource;

#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ModuleExportsAndReExports {
  pub exports: Vec<String>,
  pub reexports: Vec<String>,
}

impl ParsedSource<'_> {
  /// Analyzes the ES runtime exports for require ESM.
  pub fn analyze_es_runtime_exports(&self) -> ModuleExportsAndReExports {
    let mut result = ModuleExportsAndReExports::default();

    for stmt in &self.program.body {
      match stmt {
        Statement::ImportDeclaration(_) => {}
        Statement::ExportAllDeclaration(n) => {
          if let Some(exported) = &n.exported {
            // `export * as y from './other.js'` is a named export
            result.exports.push(exported.name().to_string());
          } else {
            result.reexports.push(n.source.value.to_string());
          }
        }
        Statement::ExportNamedDeclaration(n) => {
          if let Some(decl) = &n.declaration {
            collect_declaration_exports(decl, &mut result.exports);
          }
          for s in &n.specifiers {
            result.exports.push(
              s.exported.name().to_string(),
            );
          }
        }
        Statement::ExportDefaultDeclaration(_) => {
          result.exports.push("default".to_string());
        }
        Statement::TSImportEqualsDeclaration(_)
        | Statement::TSExportAssignment(_) => {
          // ignore because it's cjs
        }
        _ => {}
      }
    }

    result
  }
}

fn collect_declaration_exports(decl: &Declaration, exports: &mut Vec<String>) {
  match decl {
    Declaration::ClassDeclaration(d) => {
      if let Some(id) = &d.id {
        exports.push(id.name.to_string());
      }
    }
    Declaration::FunctionDeclaration(d) => {
      if let Some(id) = &d.id {
        exports.push(id.name.to_string());
      }
    }
    Declaration::VariableDeclaration(d) => {
      for decl in &d.declarations {
        collect_binding_pattern_names(&decl.id, exports);
      }
    }
    Declaration::TSEnumDeclaration(d) => {
      exports.push(d.id.name.to_string());
    }
    Declaration::TSModuleDeclaration(d) => {
      match &d.id {
        TSModuleDeclarationName::Identifier(ident) => {
          exports.push(ident.name.to_string());
        }
        TSModuleDeclarationName::StringLiteral(_) => {
          // ignore
        }
      }
    }
    Declaration::TSInterfaceDeclaration(_)
    | Declaration::TSTypeAliasDeclaration(_)
    | Declaration::TSGlobalDeclaration(_)
    | Declaration::TSImportEqualsDeclaration(_) => {
      // ignore
    }
  }
}

fn collect_binding_pattern_names(
  pattern: &BindingPattern,
  names: &mut Vec<String>,
) {
  if let Some(ident) = pattern.get_binding_identifier() {
    names.push(ident.name.to_string());
  }
  // For complex patterns (object/array destructuring), we'd need to recurse,
  // but for export declarations the pattern is typically a simple identifier.
}

#[cfg(test)]
mod test {
  use std::cell::RefCell;

  use deno_media_type::MediaType;
  use oxc::allocator::Allocator;

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
      if !std::thread::panicking() {
        self.assert_empty();
      }
    }
  }

  fn parse(source: &str) -> Tester {
    let allocator = Allocator::default();
    let parsed_source = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///example.ts").unwrap(),
        text: source.into(),
        media_type: MediaType::TypeScript,
        capture_tokens: true,
        scope_analysis: false,
        maybe_source_type: None,
      },
    )
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
export function d() {}
export const e = 1, f = 2;
export { g, h1 as h };
export * as y from './other.js';
export { z } from './other.js';
",
    );
    tester.assert_exports(vec![
      "A", "d", "e", "f", "g", "h", "y", "z",
    ]);
  }

  #[test]
  fn runtime_exports_default_expr() {
    let tester = parse("export default 5;");
    tester.assert_exports(vec!["default"]);
  }

  #[test]
  fn runtime_re_export() {
    let tester = parse("export * from './other.js';");
    tester.assert_reexports(vec!["./other.js"]);
  }
}
