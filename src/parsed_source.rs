// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::fmt;
use std::sync::Arc;

use dprint_swc_ext::common::SourceRange;
use dprint_swc_ext::common::SourceRanged;
use dprint_swc_ext::common::SourceTextInfo;
use dprint_swc_ext::common::SourceTextProvider;
use dprint_swc_ext::common::StartSourcePos;
use swc_common::Mark;
use swc_ecma_ast::ModuleDecl;
use swc_ecma_ast::ModuleItem;
use swc_ecma_ast::Stmt;

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

/// If the module is an Es module or CommonJs module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleKind {
  Es,
  Cjs,
}

/// A reference to a Program.
///
/// It is generally preferable for functions to accept this over `&Program`
/// because it doesn't require cloning when only owning a `Module` or `Script`.
#[derive(Debug, Clone, Copy)]
pub enum ProgramRef<'a> {
  Module(&'a Module),
  Script(&'a Script),
}

impl<'a> ProgramRef<'a> {
  pub fn unwrap_module(&self) -> &Module {
    match self {
      ProgramRef::Module(m) => m,
      ProgramRef::Script(_) => {
        panic!("Cannot get a module when the source was a script.")
      }
    }
  }

  pub fn unwrap_script(&self) -> &Script {
    match self {
      ProgramRef::Module(_) => {
        panic!("Cannot get a script when the source was a module.")
      }
      ProgramRef::Script(s) => s,
    }
  }

  pub fn shebang(&self) -> Option<&swc_atoms::Atom> {
    match self {
      ProgramRef::Module(m) => m.shebang.as_ref(),
      ProgramRef::Script(s) => s.shebang.as_ref(),
    }
  }

  pub fn body(&self) -> impl Iterator<Item = ModuleItemRef<'a>> {
    match self {
      ProgramRef::Module(m) => Box::new(m.body.iter().map(|n| n.into()))
        as Box<dyn Iterator<Item = ModuleItemRef<'a>>>,
      ProgramRef::Script(s) => Box::new(s.body.iter().map(ModuleItemRef::Stmt)),
    }
  }

  pub fn to_owned(&self) -> Program {
    match self {
      ProgramRef::Module(m) => Program::Module((*m).clone()),
      ProgramRef::Script(s) => Program::Script((*s).clone()),
    }
  }
}

impl<'a> From<&'a Program> for ProgramRef<'a> {
  fn from(program: &'a Program) -> Self {
    match program {
      Program::Module(module) => ProgramRef::Module(module),
      Program::Script(script) => ProgramRef::Script(script),
    }
  }
}

#[cfg(feature = "visit")]
impl<'a, T: swc_ecma_visit::Visit> swc_ecma_visit::VisitWith<T>
  for ProgramRef<'a>
{
  fn visit_with(&self, visitor: &mut T) {
    match self {
      ProgramRef::Module(n) => n.visit_with(visitor),
      ProgramRef::Script(n) => n.visit_with(visitor),
    }
  }

  fn visit_children_with(&self, visitor: &mut T) {
    match self {
      ProgramRef::Module(n) => n.visit_children_with(visitor),
      ProgramRef::Script(n) => n.visit_children_with(visitor),
    }
  }
}

impl swc_common::Spanned for ProgramRef<'_> {
  // ok because we're implementing Spanned
  #[allow(clippy::disallowed_methods)]
  #[allow(clippy::disallowed_types)]
  fn span(&self) -> swc_common::Span {
    match self {
      Self::Module(m) => m.span,
      Self::Script(s) => s.span,
    }
  }
}

/// Reference to a ModuleDecl or Stmt in a Program.
///
/// This is used to allow using the same API for the top level
/// statements when working with a ProgramRef.
#[derive(Debug, Clone, Copy)]
pub enum ModuleItemRef<'a> {
  ModuleDecl(&'a ModuleDecl),
  Stmt(&'a Stmt),
}

impl swc_common::Spanned for ModuleItemRef<'_> {
  // ok because we're implementing Spanned
  #[allow(clippy::disallowed_methods)]
  #[allow(clippy::disallowed_types)]
  fn span(&self) -> swc_common::Span {
    match self {
      Self::ModuleDecl(n) => n.span(),
      Self::Stmt(n) => n.span(),
    }
  }
}

impl<'a> From<&'a ModuleItem> for ModuleItemRef<'a> {
  fn from(item: &'a ModuleItem) -> Self {
    match item {
      ModuleItem::ModuleDecl(n) => ModuleItemRef::ModuleDecl(n),
      ModuleItem::Stmt(n) => ModuleItemRef::Stmt(n),
    }
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
  pub source_text_info: Arc<once_cell::sync::OnceCell<SourceTextInfo>>,
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
pub struct ParsedSource(pub(crate) Arc<ParsedSourceInner>);

impl ParsedSource {
  /// Gets the module specifier of the module.
  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.0.specifier
  }

  /// Gets the media type of the module.
  pub fn media_type(&self) -> MediaType {
    self.0.media_type
  }

  /// Gets the text content of the module.
  pub fn text(&self) -> &Arc<str> {
    &self.0.text
  }

  /// Gets an object with pre-computed positions for lines and indexes of
  /// multi-byte chars.
  ///
  /// Note: Prefer using `.text()` over this if able because this is lazily
  /// created.
  pub fn text_info_lazy(&self) -> &SourceTextInfo {
    self
      .0
      .source_text_info
      .get_or_init(|| SourceTextInfo::new(self.text().clone()))
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
    self.0.program.clone()
  }

  /// Gets the parsed program as a reference.
  pub fn program_ref(&self) -> ProgramRef<'_> {
    match self.0.program.as_ref() {
      Program::Module(module) => ProgramRef::Module(module),
      Program::Script(script) => ProgramRef::Script(script),
    }
  }

  /// Gets the comments found in the source file.
  pub fn comments(&self) -> &MultiThreadedComments {
    &self.0.comments
  }

  /// Wrapper around globals that swc uses for transpiling.
  pub fn globals(&self) -> &Globals {
    &self.0.globals
  }

  /// Get the source's leading comments, where triple slash directives might
  /// be located.
  pub fn get_leading_comments(&self) -> Option<&Vec<Comment>> {
    self.0.comments.get_leading(self.0.program.start())
  }

  /// Gets the tokens found in the source file.
  ///
  /// This will panic if tokens were not captured during parsing.
  pub fn tokens(&self) -> &[TokenAndSpan] {
    self
      .0
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
      let mut inner = match Arc::try_unwrap(self.0) {
        Ok(inner) => inner,
        Err(arc_inner) => ParsedSourceInner {
          // all of these are/should be cheap to clone
          specifier: arc_inner.specifier.clone(),
          media_type: arc_inner.media_type,
          text: arc_inner.text.clone(),
          source_text_info: arc_inner.source_text_info.clone(),
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
      ParsedSource(Arc::new(inner))
    }
  }

  /// Gets if the source's program has scope information stored
  /// in the identifiers.
  pub fn has_scope_analysis(&self) -> bool {
    self.0.syntax_contexts.is_some()
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
    self.0.syntax_contexts.as_ref().expect("Could not get syntax context because the source was not parsed with scope analysis.")
  }

  /// Gets extra non-fatal diagnostics found while parsing.
  pub fn diagnostics(&self) -> &Vec<ParseDiagnostic> {
    &self.0.diagnostics
  }

  /// Gets if this source is a module.
  pub fn is_module(&self) -> bool {
    matches!(self.program_ref(), ProgramRef::Module(_))
  }

  /// Gets if this source is a script.
  pub fn is_script(&self) -> bool {
    matches!(self.program_ref(), ProgramRef::Script(_))
  }
}

impl<'a> SourceTextProvider<'a> for &'a ParsedSource {
  fn text(&self) -> &'a Arc<str> {
    ParsedSource::text(self)
  }

  fn start_pos(&self) -> StartSourcePos {
    StartSourcePos::START_SOURCE_POS
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
      .field("comments", &self.0.comments)
      .field("program", &self.0.program)
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
        ProgramRef::Module(module) => crate::view::ProgramRef::Module(module),
        ProgramRef::Script(script) => crate::view::ProgramRef::Script(script),
      },
      text_info: Some(self.text_info_lazy()),
      tokens: self.0.tokens.as_ref().map(|t| t as &[TokenAndSpan]),
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
