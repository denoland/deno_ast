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
    diagnostics,
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
