// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::fmt;
use std::sync::Arc;

use dprint_swc_ext::common::SourceRange;
use dprint_swc_ext::common::SourceRanged;
use dprint_swc_ext::common::StartSourcePos;
use swc_common::Mark;

use crate::comments::MultiThreadedComments;
use crate::scope_analysis_transform;
use crate::swc::ast::Module;
use crate::swc::ast::Program;
use crate::swc::ast::Script;
use crate::swc::common::comments::Comment;
use crate::swc::common::SyntaxContext;
use crate::swc::parser::token::TokenAndSpan;
use crate::MediaType;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::SourceRangedForSpanned;

#[derive(Debug, Clone)]
pub struct Marks {
  pub unresolved: Mark,
  pub top_level: Mark,
}

#[derive(Clone)]
pub struct Globals {
  marks: Marks,
  globals: Arc<crate::swc::common::Globals>,
}

impl Default for Globals {
  fn default() -> Self {
    let globals = crate::swc::common::Globals::new();
    let marks = crate::swc::common::GLOBALS.set(&globals, || Marks {
      unresolved: Mark::new(),
      top_level: Mark::fresh(Mark::root()),
    });
    Self {
      marks,
      globals: Arc::new(globals),
    }
  }
}

impl Globals {
  pub fn with<T>(&self, action: impl FnOnce(&Marks) -> T) -> T {
    crate::swc::common::GLOBALS.set(&self.globals, || action(&self.marks))
  }

  pub fn marks(&self) -> &Marks {
    &self.marks
  }
}

#[derive(Clone)]
pub(crate) struct SyntaxContexts {
  pub unresolved: SyntaxContext,
  pub top_level: SyntaxContext,
}

pub(crate) struct ParsedSourceInner {
  pub specifier: ModuleSpecifier,
  pub media_type: MediaType,
  pub text: Arc<str>,
  pub comments: MultiThreadedComments,
  pub program: Arc<Program>,
  pub tokens: Option<Arc<Vec<TokenAndSpan>>>,
  pub globals: Globals,
  pub syntax_contexts: Option<SyntaxContexts>,
  pub diagnostics: Vec<ParseDiagnostic>,
}

/// A parsed source containing an AST, comments, and possibly tokens.
///
/// Note: This struct is cheap to clone.
#[derive(Clone)]
pub struct ParsedSource {
  pub(crate) inner: Arc<ParsedSourceInner>,
}

impl ParsedSource {
  #[allow(clippy::too_many_arguments)]
  pub(crate) fn new(
    specifier: ModuleSpecifier,
    media_type: MediaType,
    text: Arc<str>,
    comments: MultiThreadedComments,
    program: Arc<Program>,
    tokens: Option<Arc<Vec<TokenAndSpan>>>,
    globals: Globals,
    syntax_contexts: Option<SyntaxContexts>,
    diagnostics: Vec<ParseDiagnostic>,
  ) -> Self {
    ParsedSource {
      inner: Arc::new(ParsedSourceInner {
        specifier,
        media_type,
        text,
        comments,
        program,
        tokens,
        globals,
        syntax_contexts,
        diagnostics,
      }),
    }
  }

  /// Gets the module specifier of the module.
  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.inner.specifier
  }

  /// Gets the media type of the module.
  pub fn media_type(&self) -> MediaType {
    self.inner.media_type
  }

  /// Gets the text content of the module.
  pub fn text(&self) -> &Arc<str> {
    &self.inner.text
  }

  /// Gets the source range of the parsed source.
  pub fn range(&self) -> SourceRange<StartSourcePos> {
    SourceRange::new(
      StartSourcePos::START_SOURCE_POS,
      StartSourcePos::START_SOURCE_POS + self.text().len(),
    )
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

  /// Wrapper around globals that swc uses for transpiling.
  pub fn globals(&self) -> &Globals {
    &self.inner.globals
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
          text: arc_inner.text.clone(),
          comments: arc_inner.comments.clone(),
          program: arc_inner.program.clone(),
          tokens: arc_inner.tokens.clone(),
          syntax_contexts: arc_inner.syntax_contexts.clone(),
          diagnostics: arc_inner.diagnostics.clone(),
          globals: arc_inner.globals.clone(),
        },
      };
      let program = match Arc::try_unwrap(inner.program) {
        Ok(program) => program,
        Err(program) => (*program).clone(),
      };
      let (program, context) =
        scope_analysis_transform(program, &inner.globals);
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
  pub fn diagnostics(&self) -> &Vec<ParseDiagnostic> {
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

impl SourceRanged for ParsedSource {
  fn start(&self) -> dprint_swc_ext::common::SourcePos {
    StartSourcePos::START_SOURCE_POS.as_source_pos()
  }

  fn end(&self) -> dprint_swc_ext::common::SourcePos {
    StartSourcePos::START_SOURCE_POS + self.text().len()
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
    self.with_view_and_text_info(
      &crate::SourceTextInfo::new(self.text().clone()),
      with_view,
    )
  }

  /// Variant of `with_view` where a `SourceTextInfo` isn't constructed.
  ///
  /// Useful if you already have a `SourceTextInfo`. Just ensure the text
  /// matches what's in the `ParsedSource` (this is verified in debug mode).
  pub fn with_view_and_text_info<'a, T>(
    &self,
    text_info: &crate::SourceTextInfo,
    with_view: impl FnOnce(crate::view::Program<'a>) -> T,
  ) -> T {
    debug_assert_eq!(*self.text(), text_info.text());
    let program_info = crate::view::ProgramInfo {
      program: match self.program_ref() {
        Program::Module(module) => crate::view::ProgramRef::Module(module),
        Program::Script(script) => crate::view::ProgramRef::Script(script),
      },
      text_info: Some(text_info),
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
    use crate::ModuleSpecifier;
    use crate::ParseParams;

    use super::*;

    let program = parse_program(ParseParams {
      specifier: ModuleSpecifier::parse("file:///my_file.js").unwrap(),
      text: "// 1\n1 + 1\n// 2".into(),
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
