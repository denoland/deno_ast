// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::fmt;
use std::sync::Arc;

use crate::comments::MultiThreadedComments;
use crate::swc::ast::Module;
use crate::swc::ast::Program;
use crate::swc::ast::Script;
use crate::swc::common::comments::Comment;
use crate::swc::common::comments::Comments;
use crate::swc::common::Spanned;
use crate::swc::parser::token::TokenAndSpan;
use crate::MediaType;
use crate::SourceTextInfo;

/// A parsed source containing an AST, comments, and possibly tokens.
#[derive(Clone)]
pub struct ParsedSource {
  specifier: String,
  media_type: MediaType,
  source: SourceTextInfo,
  comments: MultiThreadedComments,
  program: Arc<Program>,
  tokens: Option<Arc<Vec<TokenAndSpan>>>,
}

impl ParsedSource {
  pub fn new(
    specifier: String,
    media_type: MediaType,
    source: SourceTextInfo,
    comments: MultiThreadedComments,
    program: Arc<Program>,
    tokens: Option<Arc<Vec<TokenAndSpan>>>,
  ) -> Self {
    ParsedSource {
      specifier,
      media_type,
      source,
      comments,
      program,
      tokens,
    }
  }

  /// Gets the module specifier of the module.
  pub fn specifier(&self) -> &str {
    &self.specifier
  }

  /// Gets the media type of the module.
  pub fn media_type(&self) -> MediaType {
    self.media_type
  }

  /// Gets the text content of the module.
  pub fn source(&self) -> &SourceTextInfo {
    &self.source
  }

  /// Gets the parsed program.
  pub fn program(&self) -> Arc<Program> {
    self.program.clone()
  }

  /// Gets the parsed program as a reference.
  pub fn program_ref(&self) -> &Program {
    &self.program
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
    &self.comments
  }

  /// Get the source's leading comments, where triple slash directives might
  /// be located.
  pub fn get_leading_comments(&self) -> Vec<Comment> {
    self
      .comments
      .get_leading(self.program.span().lo)
      .unwrap_or_else(Vec::new)
  }

  /// Gets the tokens found in the source file.
  pub fn tokens(&self) -> &[TokenAndSpan] {
    self
      .tokens
      .as_ref()
      .expect("Tokens not found because they were not captured during parsing.")
  }
}

impl fmt::Debug for ParsedSource {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("ParsedModule")
      .field("comments", &self.comments)
      .field("program", &self.program)
      .finish()
  }
}

#[cfg(feature = "view")]
impl ParsedSource {
  /// Gets a view of the module that allows for
  pub fn with_view<'a, T>(
    &self,
    with_view: impl FnOnce(crate::view::Program<'a>) -> T,
  ) -> T {
    let program_info = crate::view::ProgramInfo {
      program: match self.program_ref() {
        Program::Module(module) => crate::view::ProgramRef::Module(module),
        Program::Script(script) => crate::view::ProgramRef::Script(script),
      },
      source_file: Some(self.source()),
      tokens: Some(self.tokens()),
      comments: Some(crate::view::Comments {
        leading: self.comments().leading_map(),
        trailing: self.comments().trailing_map(),
      }),
    };

    crate::view::with_ast_view(program_info, with_view)
  }
}
