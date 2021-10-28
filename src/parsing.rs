// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;

use crate::comments::MultiThreadedComments;
use crate::swc::ast::Module;
use crate::swc::ast::Program;
use crate::swc::ast::Script;
use crate::swc::common::comments::SingleThreadedComments;
use crate::swc::common::input::StringInput;
use crate::swc::parser::error::Error as SwcError;
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
  /// Whether to apply swc's scope analysis.
  pub scope_analysis: bool,
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
///    scope_analysis: false,
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

/// Parses a module with post processing (see docs on `parse_program_with_post_process`).
pub fn parse_module_with_post_process(
  params: ParseParams,
  post_process: impl FnOnce(Module) -> Module,
) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Module, |program| match program {
    Program::Module(module) => Program::Module(post_process(module)),
    Program::Script(_) => unreachable!(),
  })
}

/// Parses the provided information to a script.
pub fn parse_script(params: ParseParams) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Script, |p| p)
}

/// Parses a script with post processing (see docs on `parse_program_with_post_process`).
pub fn parse_script_with_post_process(
  params: ParseParams,
  post_process: impl FnOnce(Script) -> Script,
) -> Result<ParsedSource, Diagnostic> {
  parse(params, ParseMode::Script, |program| match program {
    Program::Module(_) => unreachable!(),
    Program::Script(script) => Program::Script(post_process(script)),
  })
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
  let (comments, program, tokens, errors) =
    parse_string_input(input, syntax, params.capture_tokens, parse_mode)
      .map_err(|err| Diagnostic::from_swc_error(err, &specifier, &source))?;
  let diagnostics = errors
    .into_iter()
    .map(|err| Diagnostic::from_swc_error(err, &specifier, &source))
    .collect();
  let program = post_process(program);

  let (program, top_level_context) = if params.scope_analysis {
    #[cfg(feature = "transforms")]
    {
      use crate::swc::common::Globals;
      use crate::swc::common::Mark;
      use crate::swc::common::SyntaxContext;
      use crate::swc::transforms::resolver::ts_resolver;
      use crate::swc::visit::FoldWith;

      let globals = Globals::new();
      crate::swc::common::GLOBALS.set(&globals, || {
        // This is used to apply proper "syntax context" to all AST elements.
        let top_level_mark = Mark::fresh(Mark::root());
        let program = program.fold_with(&mut ts_resolver(top_level_mark));
        let top_level_context =
          SyntaxContext::empty().apply_mark(top_level_mark);

        (program, Some(top_level_context))
      })
    }
    #[cfg(not(feature = "transforms"))]
    panic!("Cannot parse with scope analysis. Please enable the 'transforms' feature.")
  } else {
    (program, None)
  };

  Ok(ParsedSource::new(
    specifier,
    params.media_type.to_owned(),
    source,
    MultiThreadedComments::from_single_threaded(comments),
    Arc::new(program),
    tokens.map(Arc::new),
    top_level_context,
    diagnostics,
  ))
}

#[allow(clippy::type_complexity)]
fn parse_string_input(
  input: StringInput,
  syntax: Syntax,
  capture_tokens: bool,
  parse_mode: ParseMode,
) -> Result<
  (
    SingleThreadedComments,
    Program,
    Option<Vec<TokenAndSpan>>,
    Vec<SwcError>,
  ),
  SwcError,
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
    let errors = parser.take_errors();

    Ok((comments, program, Some(tokens), errors))
  } else {
    let mut parser = swc_ecmascript::parser::Parser::new_from(lexer);
    let program = match parse_mode {
      ParseMode::Program => parser.parse_program()?,
      ParseMode::Module => Program::Module(parser.parse_module()?),
      ParseMode::Script => Program::Script(parser.parse_script()?),
    };
    let errors = parser.take_errors();

    Ok((comments, program, None, errors))
  }
}

/// Gets the default `Syntax` used by `deno_ast` for the provided media type.
pub fn get_syntax(media_type: MediaType) -> Syntax {
  match media_type {
    MediaType::JavaScript | MediaType::Mjs | MediaType::Cjs => {
      Syntax::Es(get_es_config(false))
    }
    MediaType::Jsx => Syntax::Es(get_es_config(true)),
    MediaType::TypeScript | MediaType::Mts | MediaType::Cts => {
      Syntax::Typescript(get_ts_config(false, false))
    }
    MediaType::Dts | MediaType::Dmts | MediaType::Dcts => {
      Syntax::Typescript(get_ts_config(false, true))
    }
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
    decorators: false,
    decorators_before_export: false,
    fn_bind: false,
    import_assertions: true,
    static_blocks: true,
    private_in_object: true,
  }
}

/// Gets the default `TsConfig` used by `deno_ast` for the provided options.
pub fn get_ts_config(tsx: bool, dts: bool) -> TsConfig {
  TsConfig {
    decorators: true,
    dts,
    dynamic_import: true,
    tsx,
    import_assertions: true,
    no_early_errors: false,
  }
}

#[cfg(test)]
mod test {
  use crate::LineAndColumnDisplay;

  use super::*;

  #[test]
  fn should_parse_program() {
    let program = parse_program(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string("// 1\n1 + 1\n// 2".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    assert_eq!(program.specifier(), "my_file.js");
    assert_eq!(program.source().text_str(), "// 1\n1 + 1\n// 2");
    assert_eq!(program.media_type(), MediaType::JavaScript);
    assert!(matches!(
      program.script().body[0],
      crate::swc::ast::Stmt::Expr(..)
    ));
    assert_eq!(program.get_leading_comments().len(), 1);
    assert_eq!(program.get_leading_comments()[0].text, " 1");
    assert_eq!(program.tokens().len(), 3);
    assert_eq!(program.comments().get_vec().len(), 2);
  }

  #[test]
  fn should_parse_module() {
    let program = parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string("// 1\n1 + 1\n// 2".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    assert!(matches!(
      program.module().body[0],
      crate::swc::ast::ModuleItem::Stmt(..)
    ));
  }

  #[cfg(feature = "view")]
  #[test]
  fn should_parse_brand_checks_in_js() {
    use crate::view::ClassDecl;
    use crate::view::ClassMethod;
    use crate::view::NodeTrait;

    let program = parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string(
        "class T { method() { #test in this; } }".to_string(),
      ),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();

    program.with_view(|program| {
      let class_decl = program.children()[0].expect::<ClassDecl>();
      let class_method = class_decl.class.body[0].expect::<ClassMethod>();
      let method_stmt = class_method.function.body.unwrap().stmts[0];
      assert_eq!(method_stmt.text(), "#test in this;");
    });
  }

  #[test]
  #[should_panic(
    expected = "Tokens not found because they were not captured during parsing."
  )]
  fn should_panic_when_getting_tokens_and_tokens_not_captured() {
    let program = parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string("// 1\n1 + 1\n// 2".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap();
    program.tokens();
  }

  #[test]
  fn should_handle_parse_error() {
    let diagnostic = parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string("t u".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .err()
    .unwrap();
    assert_eq!(diagnostic.specifier, "my_file.js".to_string());
    assert_eq!(
      diagnostic.display_position,
      LineAndColumnDisplay {
        line_number: 1,
        column_number: 3,
      }
    );
    assert_eq!(diagnostic.message(), "Expected ';', '}' or <eof>");
  }

  #[test]
  #[should_panic(
    expected = "Could not get top level context because the source was not parsed with scope analysis."
  )]
  fn should_panic_when_getting_top_level_context_and_scope_analysis_false() {
    get_scope_analysis_false_parsed_source().top_level_context();
  }

  fn get_scope_analysis_false_parsed_source() -> ParsedSource {
    parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string("// 1\n1 + 1\n// 2".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .unwrap()
  }

  #[cfg(all(feature = "view", feature = "transforms"))]
  #[test]
  fn should_do_scope_analysis() {
    let parsed_source = parse_module(ParseParams {
      specifier: "my_file.js".to_string(),
      source: SourceTextInfo::from_string(
        "export function test() { const test = 2; test; } test()".to_string(),
      ),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: true,
    })
    .unwrap();

    parsed_source.with_view(|view| {
      use crate::view::*;

      let func_decl = view.children()[0]
        .expect::<ExportDecl>()
        .decl
        .expect::<FnDecl>();
      let func_decl_inner_expr = func_decl.function.body.unwrap().stmts[1]
        .expect::<ExprStmt>()
        .expr
        .expect::<Ident>();
      let call_expr = view.children()[1]
        .expect::<ExprStmt>()
        .expr
        .expect::<CallExpr>();
      let call_expr_id = call_expr.callee.expect::<Ident>();

      // these should be the same identifier
      assert_eq!(func_decl.ident.to_id(), call_expr_id.to_id());
      // but these shouldn't be
      assert_ne!(func_decl.ident.to_id(), func_decl_inner_expr.to_id());
    });
  }

  #[test]
  fn should_error_on_syntax_diagnostic() {
    let diagnostic = parse_ts_module("test;\nas#;").err().unwrap();
    assert_eq!(diagnostic.message(), concat!("Expected ';', '}' or <eof>"));
  }

  #[test]
  fn should_error_without_issue_when_there_exists_multi_byte_char_on_line_with_syntax_error(
  ) {
    let diagnostic = parse_ts_module(concat!(
      "test;\n",
      r#"console.log("x", `duration ${d} not in range - ${min} ≥ ${d} && ${max} ≥ ${d}`),;"#,
    )).err().unwrap();
    assert_eq!(
      diagnostic.message(),
      concat!(
        "Unexpected token `;`. Expected this, import, async, function, [ for array literal, ",
        "{ for object literal, @ for decorator, function, class, null, true, false, number, bigint, string, ",
        "regexp, ` for template literal, (, or an identifier",
      )
    );
  }

  #[test]
  fn should_diagnostic_for_no_equals_sign_in_var_decl() {
    let diagnostic =
      parse_for_diagnostic("const Methods {\nf: (x, y) => x + y,\n};");
    assert_eq!(diagnostic.message(), "Expected a semicolon");
  }

  #[test]
  fn should_diganotic_when_var_stmts_sep_by_comma() {
    let diagnostic = parse_for_diagnostic("let a = 0, let b = 1;");
    assert_eq!(diagnostic.message(), "Expected a semicolon");
  }

  #[test]
  fn should_diagnostic_for_exected_expr_type_alias() {
    let diagnostic =
      parse_for_diagnostic("type T =\n  | unknown\n  { } & unknown;");
    assert_eq!(diagnostic.message(), "Expression expected");
  }

  fn parse_for_diagnostic(text: &str) -> Diagnostic {
    let result = parse_ts_module(text).unwrap();
    result.diagnostics().first().unwrap().to_owned()
  }

  fn parse_ts_module(text: &str) -> Result<ParsedSource, Diagnostic> {
    parse_module(ParseParams {
      specifier: "my_file.ts".to_string(),
      source: SourceTextInfo::from_string(text.to_string()),
      media_type: MediaType::TypeScript,
      capture_tokens: false,
      maybe_syntax: None,
      scope_analysis: false,
    })
  }
}
