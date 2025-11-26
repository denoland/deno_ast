// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_error::JsError;
use swc_ecma_lexer::TsSyntax;
use thiserror::Error;
use url::Url;

use crate::diagnostics::DiagnosticCollector;
use crate::diagnostics::SwcFoldDiagnosticsError;
use crate::diagnostics::ensure_no_fatal_swc_diagnostics;
use crate::swc::common::SourceMap;
use crate::swc::common::sync::Lrc;

pub struct TypeStripOptions {
  pub module: Option<bool>,
}

#[derive(Debug, Error, JsError)]
pub enum TypeStripError {
  #[class(type)]
  #[error(transparent)]
  Fold(#[from] SwcFoldDiagnosticsError),
  #[class(type)]
  #[error(transparent)]
  TsError(#[from] swc_ts_fast_strip::TsError),
}

/// Type stripts the given source.
pub fn type_strip(
  filename: &Url,
  code: String,
  options: TypeStripOptions,
) -> Result<String, TypeStripError> {
  let source_map = Lrc::new(SourceMap::default());
  let emitter = DiagnosticCollector::default();
  let (handler, diagnostics_cell) = emitter.into_handler_and_cell();
  let output = crate::swc::common::errors::HANDLER.set(&handler, || {
    swc_ts_fast_strip::operate(
      &source_map,
      &handler,
      code,
      swc_ts_fast_strip::Options {
        module: options.module,
        filename: Some(filename.to_string()),
        parser: TsSyntax {
          decorators: false,
          disallow_ambiguous_jsx_like: true,
          dts: false,
          no_early_errors: true,
          tsx: false,
        },
        mode: swc_ts_fast_strip::Mode::StripOnly,
        transform: None,
        deprecated_ts_module_as_error: Some(true),
        source_map: false,
      },
    )
  })?;

  let mut diagnostics = diagnostics_cell.borrow_mut();
  let diagnostics = std::mem::take(&mut *diagnostics);
  ensure_no_fatal_swc_diagnostics(&source_map, diagnostics.into_iter())?;
  Ok(output.code)
}

#[cfg(test)]
mod tests {
  use super::*;

  fn strip(code: &str) -> Result<String, TypeStripError> {
    type_strip(
      &Url::parse("file:///test.ts").unwrap(),
      code.to_string(),
      TypeStripOptions { module: None },
    )
  }

  #[test]
  fn test_type_strip_basic_types() {
    let code = "const x: number = 5;";
    let result = strip(code).unwrap();
    assert_eq!(result, "const x         = 5;");
  }

  #[test]
  fn test_type_strip_function_types() {
    let code = "function foo(a: string, b: number): boolean { return true; }";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "function foo(a        , b        )          { return true; }"
    );
  }

  #[test]
  fn test_type_strip_interface() {
    let code = "interface Person {\n  name: string;\n  age: number;\n}\nconst p: Person = { name: \"Alice\", age: 30 };";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "                  \n               \n              \n \nconst p         = { name: \"Alice\", age: 30 };"
    );
  }

  #[test]
  fn test_type_strip_type_alias() {
    let code = "type MyType = string | number;\nconst x: MyType = \"hello\";";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "                              \nconst x         = \"hello\";"
    );
  }

  #[test]
  fn test_type_strip_generics() {
    let code = "function identity<T>(arg: T): T { return arg; }";
    let result = strip(code).unwrap();
    assert_eq!(result, "function identity   (arg   )    { return arg; }");
  }

  #[test]
  fn test_type_strip_class() {
    let code = "class MyClass {\n  private x: number;\n  constructor(x: number) {\n    this.x = x;\n  }\n  getValue(): number {\n    return this.x;\n  }\n}";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "class MyClass {\n          x        ;\n  constructor(x        ) {\n    this.x = x;\n  }\n  getValue()         {\n    return this.x;\n  }\n}"
    );
  }

  #[test]
  fn test_type_strip_arrow_function() {
    let code =
      "const fn = (a: string, b: number): string => { return a + b; };";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "const fn = (a        , b        )         => { return a + b; };"
    );
  }

  #[test]
  fn test_type_strip_enum() {
    let code =
      "enum Color {\n  Red,\n  Green,\n  Blue\n}\nconst c: Color = Color.Red;";
    let result = strip(code);
    // Enums are not supported in strip-only mode
    assert!(result.is_err());
  }

  #[test]
  fn test_type_strip_as_assertion() {
    let code = "const x = \"hello\" as string;";
    let result = strip(code).unwrap();
    assert_eq!(result, "const x = \"hello\"          ;");
  }

  #[test]
  fn test_type_strip_non_null_assertion() {
    let code = "const x = value!;";
    let result = strip(code).unwrap();
    assert_eq!(result, "const x = value ;");
  }

  #[test]
  fn test_type_strip_import_type() {
    let code = "import type { MyType } from \"./types\";";
    let result = strip(code).unwrap();
    assert_eq!(result, "                                      ");
  }

  #[test]
  fn test_type_strip_preserves_runtime_code() {
    let code = "const x: number = 5;\nconsole.log(x);\nfunction add(a: number, b: number): number {\n  return a + b;\n}\nadd(1, 2);";
    let result = strip(code).unwrap();
    assert_eq!(
      result,
      "const x         = 5;\nconsole.log(x);\nfunction add(a        , b        )         {\n  return a + b;\n}\nadd(1, 2);"
    );
  }

  #[test]
  fn test_type_strip_optional_params() {
    let code = "function foo(a?: string, b?: number) { return a; }";
    let result = strip(code).unwrap();
    assert_eq!(result, "function foo(a         , b         ) { return a; }");
  }

  #[test]
  fn test_type_strip_module_option() {
    let code = "const x: number = 5;";
    let result = type_strip(
      &Url::parse("file:///test.ts").unwrap(),
      code.to_string(),
      TypeStripOptions { module: Some(true) },
    )
    .unwrap();
    assert_eq!(result, "const x         = 5;");
  }

  #[test]
  fn test_type_strip_syntax_error() {
    let code = "const x: = 5;";
    let result = strip(code);
    assert!(result.is_err());
  }

  #[test]
  fn test_type_strip_empty_code() {
    let code = "";
    let result = strip(code).unwrap();
    assert_eq!(result, "");
  }

  #[test]
  fn test_type_strip_namespace() {
    let code = "namespace MyNamespace {\n  export const x: number = 5;\n}";
    let result = strip(code);
    // This should error due to deprecated_ts_module_as_error being true
    assert!(result.is_err());
  }
}
