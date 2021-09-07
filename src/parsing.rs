// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use crate::comments::MultiThreadedComments;
use crate::swc::ast::Program;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::input::StringInput;
use crate::swc::common::Spanned;
use crate::swc::parser::lexer::Lexer;
use crate::swc::parser::token::TokenAndSpan;
use crate::swc::parser::EsConfig;
use crate::swc::parser::JscTarget;
use crate::swc::parser::Syntax;
use crate::swc::parser::TsConfig;
use crate::Diagnostic;
use crate::MediaType;
use crate::ParsedSource;
use crate::SourceTextInfo;

/// Ecmascript version used for lexing and parsing.
pub const TARGET: JscTarget = JscTarget::Es2021;

/// Parameters for parsing.
pub struct ParseParams {
  /// Specifier of the source text.
  pub specifier: String,
  /// Source text stored in a `SourceTextInfo`.
  pub source: SourceTextInfo,
  /// Media type of the source text.
  pub media_type: MediaType,
  /// Whether to capture tokens or not.
  pub capture_tokens: bool,
  /// Syntax to use when parsing.
  ///
  /// `deno_ast` will get a default `Syntax` to use based on the
  /// media type, but you may use this to provide a custom `Syntax`.
  pub maybe_syntax: Option<Syntax>,
}

/// Parses the provided information attempting to figure out if the provided
/// text is for a script or a module.
pub fn parse_program(params: ParseParams) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Program, |p| p)
}

/// Parses the provided information as a program with the option of providing some
/// post-processing to the result.
///
/// # Example
///
/// ```
/// deno_ast::parse_program_with_post_process(
///  deno_ast::ParseParams {
///    specifier: "file:///my_file.ts".to_string(),
///    media_type: deno_ast::MediaType::TypeScript,
///    source: deno_ast::SourceTextInfo::from_string("".to_string()),
///    capture_tokens: true,
///    maybe_syntax: None,
///  },
///  |program| {
///    // do something with the program here before it gets stored
///    program
///  },
/// );
/// ```
pub fn parse_program_with_post_process(
  params: ParseParams,
  post_process: impl FnOnce(Program) -> Program,
) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Program, post_process)
}

/// Parses the provided information to a module.
pub fn parse_module(params: ParseParams) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Module, |p| p)
}

/// Parses the provided information to a script.
pub fn parse_script(params: ParseParams) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Script, |p| p)
}

enum ParseMode {
  Program,
  Module,
  Script,
}

fn parse(
  params: ParseParams,
  parse_mode: ParseMode,
  post_process: impl FnOnce(Program) -> Program,
) -> Result<ParsedSource, Diagnostic> {
  let source = params.source;
  let source_span = source.span();
  let specifier = params.specifier;
  let input =
    StringInput::new(source.text_str(), source_span.lo(), source_span.hi());
  let media_type = params.media_type;
  let syntax = params
    .maybe_syntax
    .unwrap_or_else(|| get_syntax(media_type));
  let (comments, program, tokens) =
    parse_string_input(input, syntax, params.capture_tokens, parse_mode)
      .map_err(|err| Diagnostic {
        display_position: source.line_and_column_display(err.span().lo),
        specifier: specifier.clone(),
        message: err.into_kind().msg().to_string(),
      })?;
  let program = post_process(program);

  Ok(ParsedSource::new(
    specifier,
    params.media_type.to_owned(),
    source,
    MultiThreadedComments::from_single_threaded(comments),
    Arc::new(program),
    tokens.map(Arc::new),
  ))
}

fn parse_string_input(
  input: StringInput,
  syntax: Syntax,
  capture_tokens: bool,
  parse_mode: ParseMode,
) -> Result<
  (SingleThreadedComments, Program, Option<Vec<TokenAndSpan>>),
  swc_ecmascript::parser::error::Error,
> {
  let comments = SingleThreadedComments::default();
  let lexer = Lexer::new(syntax, TARGET, input, Some(&comments));

  if capture_tokens {
    let lexer = swc_ecmascript::parser::Capturing::new(lexer);
    let mut parser = swc_ecmascript::parser::Parser::new_from(lexer);
    let program = match parse_mode {
      ParseMode::Program => parser.parse_program()?,
      ParseMode::Module => Program::Module(parser.parse_module()?),
      ParseMode::Script => Program::Script(parser.parse_script()?),
    };
    let tokens = parser.input().take();

    Ok((comments, program, Some(tokens)))
  } else {
    let mut parser = swc_ecmascript::parser::Parser::new_from(lexer);
    let program = match parse_mode {
      ParseMode::Program => parser.parse_program()?,
      ParseMode::Module => Program::Module(parser.parse_module()?),
      ParseMode::Script => Program::Script(parser.parse_script()?),
    };

    Ok((comments, program, None))
  }
}

/// Gets the default `Syntax` used by `deno_ast` for the provided media type.
pub fn get_syntax(media_type: MediaType) -> Syntax {
  match media_type {
    MediaType::JavaScript => Syntax::Es(get_es_config(false)),
    MediaType::Jsx => Syntax::Es(get_es_config(true)),
    MediaType::TypeScript => Syntax::Typescript(get_ts_config(false, false)),
    MediaType::Dts => Syntax::Typescript(get_ts_config(false, true)),
    MediaType::Tsx => Syntax::Typescript(get_ts_config(true, false)),
    _ => Syntax::Es(get_es_config(false)),
  }
}

/// Gets the default `EsConfig` used by `deno_ast` for the provided options.
pub fn get_es_config(jsx: bool) -> EsConfig {
  EsConfig {
    class_private_methods: true,
    class_private_props: true,
    class_props: true,
    dynamic_import: true,
    export_default_from: true,
    export_namespace_from: true,
    import_meta: true,
    jsx,
    nullish_coalescing: true,
    num_sep: true,
    optional_chaining: true,
    top_level_await: true,
    ..EsConfig::default()
  }
}

/// Gets the default `TsConfig` used by `deno_ast` for the provided options.
pub fn get_ts_config(tsx: bool, dts: bool) -> TsConfig {
  TsConfig {
    decorators: true,
    dts,
    dynamic_import: true,
    tsx,
    ..TsConfig::default()
  }
}
