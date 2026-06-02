// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::fmt;
use std::sync::Arc;
use std::sync::OnceLock;

use oxc::ast::ast::Program;
use oxc::ast::ast::Statement;
use oxc::span::Span;

use crate::MediaType;
use crate::ModuleSpecifier;
use crate::ParseDiagnostic;
use crate::SourceTextInfo;

/// If the module is an ES module or CommonJs module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ModuleKind {
  Esm,
  Cjs,
}

impl ModuleKind {
  #[inline(always)]
  pub fn from_is_cjs(is_cjs: bool) -> Self {
    if is_cjs {
      ModuleKind::Cjs
    } else {
      ModuleKind::Esm
    }
  }

  #[inline(always)]
  pub fn from_is_esm(is_esm: bool) -> Self {
    ModuleKind::from_is_cjs(!is_esm)
  }

  #[inline(always)]
  pub fn is_cjs(&self) -> bool {
    matches!(self, Self::Cjs)
  }

  #[inline(always)]
  pub fn is_esm(&self) -> bool {
    matches!(self, Self::Esm)
  }
}

#[derive(Debug, Clone, Default)]
pub struct ParseDiagnostics {
  pub diagnostics: Vec<ParseDiagnostic>,
  /// Diagnostics that should be surfaced when transpiling if the
  /// file parsed as a script is discovered to be a module.
  pub script_module_diagnostics: Vec<ParseDiagnostic>,
}

impl ParseDiagnostics {
  #[cfg(feature = "transpiling")]
  pub fn for_module_kind<'a>(
    &'a self,
    module_kind: ModuleKind,
  ) -> Box<dyn Iterator<Item = &'a ParseDiagnostic> + 'a> {
    match module_kind {
      ModuleKind::Esm => Box::new(
        self
          .diagnostics
          .iter()
          .chain(self.script_module_diagnostics.iter()),
      ),
      ModuleKind::Cjs => Box::new(self.diagnostics.iter()),
    }
  }
}

/// A parsed source containing an AST, comments, and possibly tokens.
///
/// This struct borrows from the allocator used during parsing.
pub struct ParsedSource<'a> {
  pub(crate) specifier: ModuleSpecifier,
  pub(crate) media_type: MediaType,
  pub(crate) text: Arc<str>,
  pub(crate) source_text_info: OnceLock<SourceTextInfo>,
  pub(crate) program: Program<'a>,
  pub(crate) tokens: oxc::allocator::Vec<'a, oxc::parser::Token>,
  pub(crate) diagnostics: ParseDiagnostics,
  /// Scope/symbol information, populated when parsing with
  /// `scope_analysis: true`. Symbol/reference ids on AST nodes are also set
  /// during semantic analysis.
  pub(crate) scoping: Option<oxc::semantic::Scoping>,
}

impl<'a> ParsedSource<'a> {
  /// Gets the module specifier of the module.
  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.specifier
  }

  /// Gets the media type of the module.
  pub fn media_type(&self) -> MediaType {
    self.media_type
  }

  /// Gets the text content of the module.
  pub fn text(&self) -> &Arc<str> {
    &self.text
  }

  /// Gets an object with pre-computed positions for lines and indexes.
  ///
  /// Note: Prefer using `.text()` over this if able because this is lazily
  /// created.
  pub fn text_info_lazy(&self) -> &SourceTextInfo {
    self
      .source_text_info
      .get_or_init(|| SourceTextInfo::new(self.text.clone()))
  }

  /// Gets the parsed program.
  pub fn program(&self) -> &Program<'a> {
    &self.program
  }

  /// Gets the scope/symbol information, if the source was parsed with
  /// `scope_analysis: true`.
  pub fn scoping(&self) -> Option<&oxc::semantic::Scoping> {
    self.scoping.as_ref()
  }

  /// Takes ownership of the scope/symbol information, if present.
  pub fn take_scoping(&mut self) -> Option<oxc::semantic::Scoping> {
    self.scoping.take()
  }

  /// Gets a mutable reference to the parsed program.
  pub fn program_mut(&mut self) -> &mut Program<'a> {
    &mut self.program
  }

  /// Gets the tokens parsed from the source file.
  pub fn tokens(&self) -> &oxc::allocator::Vec<'a, oxc::parser::Token> {
    &self.tokens
  }

  /// Gets the comments found in the source file.
  pub fn comments(&self) -> &oxc::allocator::Vec<'a, oxc::ast::ast::Comment> {
    &self.program.comments
  }

  /// Gets the top-level statements.
  pub fn body(&self) -> &oxc::allocator::Vec<'a, Statement<'a>> {
    &self.program.body
  }

  /// Gets the span of the program.
  pub fn span(&self) -> Span {
    self.program.span
  }

  /// Gets whether the source was parsed as a module.
  pub fn is_module(&self) -> bool {
    self.program.source_type.is_module()
  }

  /// Gets whether the source was parsed as a script.
  pub fn is_script(&self) -> bool {
    self.program.source_type.is_script()
  }

  /// Computes whether this program should be treated as a script.
  ///
  /// For TypeScript CJS files, the parser may parse as module but
  /// we need to detect if it's actually a script (e.g. `import x = require()`).
  pub fn compute_is_script(&self) -> bool {
    if self.media_type.is_typed() {
      compute_is_script_from_body(&self.program.body)
    } else {
      self.is_script()
    }
  }

  /// Gets extra non-fatal diagnostics found while parsing.
  pub fn diagnostics(&self) -> &Vec<ParseDiagnostic> {
    &self.diagnostics.diagnostics
  }

  /// Diagnostics that should be surfaced when transpiling if the
  /// file parsed as a script is discovered to be a module.
  pub fn script_module_diagnostics(&self) -> &Vec<ParseDiagnostic> {
    &self.diagnostics.script_module_diagnostics
  }

  /// Get the source's leading comments, where triple slash directives might
  /// be located.
  pub fn get_leading_comments(
    &self,
  ) -> impl Iterator<Item = &oxc::ast::ast::Comment> {
    // Leading comments are comments that appear before the first statement.
    let first_stmt_start = self
      .program
      .body
      .first()
      .map(|s| {
        use oxc::span::GetSpan;
        s.span().start
      })
      .unwrap_or(self.program.span.end);

    self
      .program
      .comments
      .iter()
      .filter(move |c| c.span.start < first_stmt_start)
  }
}

/// Checks if a module body should be treated as a script by looking
/// for TypeScript-specific CJS patterns like `import x = require()`.
fn compute_is_script_from_body(body: &[Statement<'_>]) -> bool {
  use oxc::ast::ast::*;
  for stmt in body {
    match stmt {
      Statement::ImportDeclaration(_)
      | Statement::ExportNamedDeclaration(_)
      | Statement::ExportDefaultDeclaration(_)
      | Statement::ExportAllDeclaration(_) => return false,
      Statement::TSImportEqualsDeclaration(_)
      | Statement::TSExportAssignment(_) => return true,
      _ => {}
    }
  }
  false
}

impl fmt::Debug for ParsedSource<'_> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_struct("ParsedSource")
      .field("specifier", &self.specifier)
      .field("media_type", &self.media_type)
      .finish()
  }
}
