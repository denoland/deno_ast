// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::fmt;
use std::sync::Arc;

use crate::comments::MultiThreadedComments;
use crate::scope_analysis_transform;
use crate::swc::ast::Module;
use crate::swc::ast::Program;
use crate::swc::ast::Script;
use crate::swc::common::comments::Comment;
use crate::swc::common::SyntaxContext;
use crate::swc::parser::token::TokenAndSpan;
use crate::Diagnostic;
use crate::MediaType;
use crate::SourceRangedForSpanned;
use crate::SourceTextInfo;

#[derive(Clone)]
pub(crate) struct SyntaxContexts {
  pub unresolved: SyntaxContext,
  pub top_level: SyntaxContext,
}

struct ParsedSourceInner {
  specifier: String,
  media_type: MediaType,
  text_info: SourceTextInfo,
  comments: MultiThreadedComments,
  program: Arc<Program>,
  tokens: Option<Arc<Vec<TokenAndSpan>>>,
  syntax_contexts: Option<SyntaxContexts>,
  diagnostics: Vec<Diagnostic>,
}

/// A parsed source containing an AST, comments, and possibly tokens.
///
/// Note: This struct is cheap to clone.
#[derive(Clone)]
pub struct ParsedSource {
  inner: Arc<ParsedSourceInner>,
}

impl ParsedSource {
  #[allow(clippy::too_many_arguments)]
  pub(crate) fn new(
    specifier: String,
    media_type: MediaType,
    text_info: SourceTextInfo,
    comments: MultiThreadedComments,
    program: Arc<Program>,
    tokens: Option<Arc<Vec<TokenAndSpan>>>,
    syntax_contexts: Option<SyntaxContexts>,
    diagnostics: Vec<Diagnostic>,
  ) -> Self {
    ParsedSource {
      inner: Arc::new(ParsedSourceInner {
        specifier,
        media_type,
        text_info,
        comments,
        program,
        tokens,
        syntax_contexts,
        diagnostics,
      }),
    }
  }

  /// Gets the module specifier of the module.
  pub fn specifier(&self) -> &str {
    &self.inner.specifier
  }

  /// Gets the media type of the module.
  pub fn media_type(&self) -> MediaType {
    self.inner.media_type
  }

  /// Gets the text content of the module.
  pub fn text_info(&self) -> &SourceTextInfo {
    &self.inner.text_info
  }

  /// Gets the parsed program.
  pub fn program(&self) -> Arc<Program> {
    self.inner.program.clone()
  }

  /// Gets the parsed program as a reference.
  pub fn program_ref(&self) -> &Program {
    &self.inner.program
  }

  /// Gets the parsed module.
  ///
  /// This will panic if the source is not a module.
  pub fn module(&self) -> &Module {
    match self.program_ref() {
      Program::Module(module) => module,
      Program::Script(_) => panic!("Cannot get a module when the source was a script. Use `.program()` instead."),
    }
  }

  /// Gets the parsed script.
  ///
  /// This will panic if the source is not a script.
  pub fn script(&self) -> &Script {
    match self.program_ref() {
      Program::Script(script) => script,
      Program::Module(_) => panic!("Cannot get a script when the source was a module. Use `.program()` instead."),
    }
  }

  /// Gets the comments found in the source file.
  pub fn comments(&self) -> &MultiThreadedComments {
    &self.inner.comments
  }

  /// Get the source's leading comments, where triple slash directives might
  /// be located.
  pub fn get_leading_comments(&self) -> Option<&Vec<Comment>> {
    self.inner.comments.get_leading(self.inner.program.start())
  }

  /// Gets the tokens found in the source file.
  ///
  /// This will panic if tokens were not captured during parsing.
  pub fn tokens(&self) -> &[TokenAndSpan] {
    self
      .inner
      .tokens
      .as_ref()
      .expect("Tokens not found because they were not captured during parsing.")
  }

  /// Adds scope analysis to the parsed source if not parsed
  /// with scope analysis.
  ///
  /// Note: This will attempt to not clone the underlying data, but
  /// will clone if multiple clones of the `ParsedSource` exist.
  pub fn into_with_scope_analysis(self) -> Self {
    if self.has_scope_analysis() {
      self
    } else {
      let mut inner = match Arc::try_unwrap(self.inner) {
        Ok(inner) => inner,
        Err(arc_inner) => ParsedSourceInner {
          // all of these are/should be cheap to clone
          specifier: arc_inner.specifier.clone(),
          media_type: arc_inner.media_type,
          text_info: arc_inner.text_info.clone(),
          comments: arc_inner.comments.clone(),
          program: arc_inner.program.clone(),
          tokens: arc_inner.tokens.clone(),
          syntax_contexts: arc_inner.syntax_contexts.clone(),
          diagnostics: arc_inner.diagnostics.clone(),
        },
      };
      let program = match Arc::try_unwrap(inner.program) {
        Ok(program) => program,
        Err(program) => (*program).clone(),
      };
      let (program, context) = scope_analysis_transform(program);
      inner.program = Arc::new(program);
      inner.syntax_contexts = context;
      ParsedSource {
        inner: Arc::new(inner),
      }
    }
  }

  /// Gets if the source's program has scope information stored
  /// in the identifiers.
  pub fn has_scope_analysis(&self) -> bool {
    self.inner.syntax_contexts.is_some()
  }

  /// Gets the top level context used when parsing with scope analysis.
  ///
  /// This will panic if the source was not parsed with scope analysis.
  pub fn top_level_context(&self) -> SyntaxContext {
    self.syntax_contexts().top_level
  }

  /// Gets the unresolved context used when parsing with scope analysis.
  ///
  /// This will panic if the source was not parsed with scope analysis.
  pub fn unresolved_context(&self) -> SyntaxContext {
    self.syntax_contexts().unresolved
  }

  fn syntax_contexts(&self) -> &SyntaxContexts {
    self.inner.syntax_contexts.as_ref().expect("Could not get syntax context because the source was not parsed with scope analysis.")
  }

  /// Gets extra non-fatal diagnostics found while parsing.
  pub fn diagnostics(&self) -> &Vec<Diagnostic> {
    &self.inner.diagnostics
  }

  /// Gets if this source is a module.
  pub fn is_module(&self) -> bool {
    matches!(self.program_ref(), Program::Module(_))
  }

  /// Gets if this source is a script.
  pub fn is_script(&self) -> bool {
    matches!(self.program_ref(), Program::Script(_))
  }
}

impl fmt::Debug for ParsedSource {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("ParsedModule")
      .field("comments", &self.inner.comments)
      .field("program", &self.inner.program)
      .finish()
  }
}

#[cfg(feature = "view")]
impl ParsedSource {
  /// Gets a dprint-swc-ext view of the module.
  ///
  /// This provides a closure to examine an "ast view" of the swc AST
  /// which has more helper methods and allows for going up the ancestors
  /// of a node.
  ///
  /// Read more: https://github.com/dprint/dprint-swc-ext
  pub fn with_view<'a, T>(
    &self,
    with_view: impl FnOnce(crate::view::Program<'a>) -> T,
  ) -> T {
    let program_info = crate::view::ProgramInfo {
      program: match self.program_ref() {
        Program::Module(module) => crate::view::ProgramRef::Module(module),
        Program::Script(script) => crate::view::ProgramRef::Script(script),
      },
      text_info: Some(self.text_info()),
      tokens: self.inner.tokens.as_ref().map(|t| t as &[TokenAndSpan]),
      comments: Some(crate::view::Comments {
        leading: self.comments().leading_map(),
        trailing: self.comments().trailing_map(),
      }),
    };

    crate::view::with_ast_view(program_info, with_view)
  }
}

#[cfg(test)]
mod test {
  #[cfg(feature = "view")]
  #[test]
  fn should_parse_program() {
    use crate::parse_program;
    use crate::view::NodeTrait;
    use crate::ParseParams;

    use super::*;

    let program = parse_program(ParseParams {
      specifier: "my_file.js".to_string(),
      text_info: SourceTextInfo::from_string("// 1\n1 + 1\n// 2".to_string()),
      media_type: MediaType::JavaScript,
      capture_tokens: true,
      maybe_syntax: None,
      scope_analysis: false,
    })
    .expect("should parse");

    let result = program.with_view(|program| {
      assert_eq!(program.children().len(), 1);
      assert_eq!(program.children()[0].text(), "1 + 1");

      2
    });

    assert_eq!(result, 2);
  }
}
