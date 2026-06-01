// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use oxc::allocator::Allocator;
use oxc::parser::{ParseOptions, Parser};
use oxc::span::SourceType;

use crate::MediaType;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::ParseDiagnostics;
use crate::ParsedSource;
use crate::SourceTextInfo;

/// Parameters for parsing.
pub struct ParseParams {
  /// Specifier of the source text.
  pub specifier: ModuleSpecifier,
  /// Source text.
  pub text: Arc<str>,
  /// Media type of the source text.
  pub media_type: MediaType,
  /// Whether to capture tokens or not.
  pub capture_tokens: bool,
  /// Whether to apply scope analysis.
  pub scope_analysis: bool,
  /// Source type override. If `None`, will be derived from media type.
  pub maybe_source_type: Option<SourceType>,
}

/// Parses the provided information attempting to figure out if the provided
/// text is for a script or a module.
pub fn parse_program<'a>(
  allocator: &'a Allocator,
  params: ParseParams,
) -> Result<ParsedSource<'a>, ParseDiagnostic> {
  parse(allocator, params, None)
}

/// Parses the provided information to a module.
pub fn parse_module<'a>(
  allocator: &'a Allocator,
  params: ParseParams,
) -> Result<ParsedSource<'a>, ParseDiagnostic> {
  parse(allocator, params, Some(true))
}

/// Parses the provided information to a script.
pub fn parse_script<'a>(
  allocator: &'a Allocator,
  params: ParseParams,
) -> Result<ParsedSource<'a>, ParseDiagnostic> {
  parse(allocator, params, Some(false))
}

fn parse<'a>(
  allocator: &'a Allocator,
  params: ParseParams,
  force_module: Option<bool>,
) -> Result<ParsedSource<'a>, ParseDiagnostic> {
  let source = strip_bom_from_arc(params.text, /* panic in debug */ true);
  let specifier = params.specifier;
  let media_type = params.media_type;

  let source_type = params
    .maybe_source_type
    .unwrap_or_else(|| get_source_type(media_type));

  // Apply force_module override
  let source_type = match force_module {
    Some(true) => source_type.with_module(true),
    Some(false) => source_type.with_script(true),
    None => {
      // Refine based on media type
      match media_type {
        MediaType::Cjs => source_type.with_script(true),
        MediaType::Cts => {
          // cts files can contain module declarations like
          // `import x = require("./x.ts");`, so parse as module
          source_type.with_module(true)
        }
        MediaType::Mjs | MediaType::Mts => source_type.with_module(true),
        _ => source_type,
      }
    }
  };

  let parse_options = ParseOptions {
    parse_regular_expression: false,
    ..Default::default()
  };

  // Allocate source text in the arena so it lives for 'a
  let source_in_arena = allocator.alloc_str(source.as_ref());

  let ret = Parser::new(allocator, source_in_arena, source_type)
    .with_options(parse_options)
    .parse();

  // Check for fatal parse errors
  if ret.panicked {
    let source_text_info = SourceTextInfo::new(source.clone());
    if let Some(error) = ret.errors.into_iter().next() {
      return Err(ParseDiagnostic::from_oxc_diagnostic(
        error,
        &specifier,
        source_text_info,
      ));
    }
    return Err(ParseDiagnostic::from_message(
      "Parser panicked".to_string(),
      &specifier,
      SourceTextInfo::new(source.clone()),
      oxc::span::Span::new(0, 0),
    ));
  }

  // A script (e.g. a CommonJS module) cannot use `import`/`export` statements.
  // OXC parses these without erroring but records `has_module_syntax`, so
  // surface the error here rather than silently accepting the module syntax.
  if source_type.is_script() && ret.module_record.has_module_syntax {
    let span = first_module_decl_span(&ret.program)
      .unwrap_or_else(|| oxc::span::Span::new(0, 0));
    return Err(ParseDiagnostic::from_message(
      "'import', and 'export' cannot be used outside of module code"
        .to_string(),
      &specifier,
      SourceTextInfo::new(source.clone()),
      span,
    ));
  }

  // Collect non-fatal diagnostics
  let diagnostics = if ret.errors.is_empty() {
    ParseDiagnostics::default()
  } else {
    let source_text_info = SourceTextInfo::new(source.clone());
    let diags = ret
      .errors
      .into_iter()
      .map(|err| {
        ParseDiagnostic::from_oxc_diagnostic(
          err,
          &specifier,
          source_text_info.clone(),
        )
      })
      .collect();
    ParseDiagnostics {
      diagnostics: diags,
      script_module_diagnostics: Vec::new(),
    }
  };

  Ok(ParsedSource {
    specifier,
    media_type,
    text: source,
    source_text_info: Default::default(),
    program: ret.program,
    tokens: ret.tokens,
    diagnostics,
  })
}

/// Finds the span of the first `import`/`export` statement in a program, used
/// to point at module syntax that appears in a script.
fn first_module_decl_span(
  program: &oxc::ast::ast::Program,
) -> Option<oxc::span::Span> {
  use oxc::ast::ast::Statement;
  program.body.iter().find_map(|stmt| match stmt {
    Statement::ImportDeclaration(d) => Some(d.span),
    Statement::ExportAllDeclaration(d) => Some(d.span),
    Statement::ExportDefaultDeclaration(d) => Some(d.span),
    Statement::ExportNamedDeclaration(d) => Some(d.span),
    Statement::TSExportAssignment(d) => Some(d.span),
    Statement::TSNamespaceExportDeclaration(d) => Some(d.span),
    _ => None,
  })
}

/// Gets the default `SourceType` used by `deno_ast` for the provided media type.
pub fn get_source_type(media_type: MediaType) -> SourceType {
  match media_type {
    MediaType::TypeScript | MediaType::Mts | MediaType::Cts => SourceType::ts(),
    MediaType::Tsx => SourceType::tsx(),
    MediaType::Dts | MediaType::Dmts | MediaType::Dcts => {
      SourceType::ts().with_typescript_definition(true)
    }
    MediaType::Jsx => SourceType::jsx(),
    MediaType::JavaScript => SourceType::unambiguous(),
    MediaType::Mjs => SourceType::mjs(),
    MediaType::Cjs => SourceType::cjs(),
    MediaType::Json
    | MediaType::Jsonc
    | MediaType::Json5
    | MediaType::Markdown
    | MediaType::Wasm
    | MediaType::SourceMap
    | MediaType::Css
    | MediaType::Sql
    | MediaType::Html
    | MediaType::Unknown => SourceType::mjs(),
  }
}

pub fn strip_bom(mut s: String) -> String {
  if s.starts_with('\u{FEFF}') {
    s.drain(..3);
  }
  s
}

fn strip_bom_from_arc(s: Arc<str>, should_panic_in_debug: bool) -> Arc<str> {
  if let Some(stripped_text) = s.strip_prefix('\u{FEFF}') {
    // this is only a perf concern, so don't crash in release
    if cfg!(debug_assertions) && should_panic_in_debug {
      panic!(
        "BOM should be stripped from text before providing it to deno_ast to avoid a file text allocation"
      );
    }
    stripped_text.into()
  } else {
    s
  }
}

#[cfg(test)]
mod test {
  use pretty_assertions::assert_eq;

  use super::*;

  #[test]
  fn should_parse_program() {
    let allocator = Allocator::default();
    let program = parse_program(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: "// 1\n1 + 1\n// 2".into(),
        media_type: MediaType::JavaScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    assert_eq!(program.specifier().as_str(), "file:///my_file.js");
    assert_eq!(program.text().as_ref(), "// 1\n1 + 1\n// 2");
    assert_eq!(program.media_type(), MediaType::JavaScript);
    assert_eq!(program.body().len(), 1);
    assert_eq!(program.get_leading_comments().count(), 1);
    assert_eq!(program.comments().len(), 2);
  }

  #[test]
  fn should_parse_module() {
    let allocator = Allocator::default();
    let program = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: "// 1\n1 + 1\n// 2".into(),
        media_type: MediaType::JavaScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    assert!(program.is_module());
  }

  #[test]
  fn should_handle_parse_error() {
    let allocator = Allocator::default();
    let diagnostic = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: "t u".into(),
        media_type: MediaType::JavaScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .err()
    .unwrap();
    assert_eq!(diagnostic.specifier().as_str(), "file:///my_file.js");
  }

  #[test]
  fn should_error_on_module_syntax_in_script() {
    let allocator = Allocator::default();
    let diagnostic = parse_program(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.cjs").unwrap(),
        text: "export class Test {}".into(),
        media_type: MediaType::Cjs,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .err()
    .unwrap();
    assert_eq!(
      diagnostic.message(),
      "'import', and 'export' cannot be used outside of module code"
    );
  }

  #[test]
  fn should_get_leading_comments_after_hashbang() {
    let allocator = Allocator::default();
    let program = parse_program(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: "#!/bin/sh deno\n// 1\n".into(),
        media_type: MediaType::JavaScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .unwrap();
    assert_eq!(program.get_leading_comments().count(), 1);
  }

  #[test]
  fn test_parse_export_equals() {
    let allocator = Allocator::default();
    assert!(
      parse_program(
        &allocator,
        ParseParams {
          specifier: ModuleSpecifier::parse("file:///my_file.ts").unwrap(),
          text: "export = 5;".into(),
          media_type: MediaType::Cts,
          capture_tokens: false,
          maybe_source_type: None,
          scope_analysis: false,
        },
      )
      .is_ok()
    );
  }

  #[test]
  fn should_error_on_syntax_diagnostic() {
    let diagnostic = get_any_diagnostic("test;\nas#;");
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error()
   {
    let diagnostic = get_any_diagnostic(concat!(
      "test;\n",
      r#"console.log("x", `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
    ));
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_diagnostic_for_no_equals_sign_in_var_decl() {
    let diagnostic =
      get_any_diagnostic("const Methods {\nf: (x, y) => x + y,\n};");
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_diganotic_when_var_stmts_sep_by_comma() {
    let diagnostic = get_any_diagnostic("let a = 0, let b = 1;");
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_diagnostic_for_exected_expr_type_alias() {
    let diagnostic =
      get_any_diagnostic("type T =\n  | unknown\n  { } & unknown;");
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_diganotic_missing_init_in_using() {
    let diagnostic = get_any_diagnostic("using test");
    assert!(!diagnostic.message().is_empty());
  }

  #[test]
  fn should_handle_parse_error_display_position() {
    let allocator = Allocator::default();
    let diagnostic = parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
        text: "t u".into(),
        media_type: MediaType::JavaScript,
        capture_tokens: true,
        maybe_source_type: None,
        scope_analysis: false,
      },
    )
    .err()
    .unwrap();
    assert_eq!(
      diagnostic.display_position(),
      crate::LineAndColumnDisplay {
        line_number: 1,
        column_number: 2,
      }
    );
  }

  /// Gets a diagnostic from the source - either a fatal parse error
  /// or a non-fatal diagnostic found after parsing.
  fn get_any_diagnostic(text: &str) -> ParseDiagnostic {
    let allocator = Allocator::default();
    match parse_module(
      &allocator,
      ParseParams {
        specifier: ModuleSpecifier::parse("file:///my_file.ts").unwrap(),
        text: text.to_string().into(),
        media_type: MediaType::TypeScript,
        capture_tokens: false,
        maybe_source_type: None,
        scope_analysis: false,
      },
    ) {
      Err(diagnostic) => diagnostic,
      Ok(parsed) => parsed
        .diagnostics()
        .first()
        .expect("Expected at least one diagnostic")
        .to_owned(),
    }
  }

  #[test]
  fn test_strip_bom() {
    let text = "\u{FEFF}test";
    assert_eq!(strip_bom(text.to_string()), "test");
    let text = "test";
    assert_eq!(strip_bom(text.to_string()), "test");
    let text = "";
    assert_eq!(strip_bom(text.to_string()), "");
  }

  #[test]
  fn test_strip_bom_arc() {
    let text = "\u{FEFF}test";
    assert_eq!(strip_bom_from_arc(text.into(), false), "test".into());
    let text = "test";
    assert_eq!(strip_bom_from_arc(text.into(), false), "test".into());
    let text = "";
    assert_eq!(strip_bom_from_arc(text.into(), false), "".into());
  }
}
