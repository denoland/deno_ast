// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::ModuleSpecifier;
use crate::SourceTextInfo;
use crate::diagnostics::Diagnostic;
use crate::diagnostics::DiagnosticLevel;
use crate::diagnostics::DiagnosticLocation;
use crate::diagnostics::DiagnosticSnippet;
use crate::diagnostics::DiagnosticSnippetHighlight;
use crate::diagnostics::DiagnosticSnippetHighlightStyle;
use crate::diagnostics::DiagnosticSourcePos;
use crate::diagnostics::DiagnosticSourceRange;
use crate::LineAndColumnDisplay;
use deno_error::JsError;
use oxc::diagnostics::OxcDiagnostic;
use oxc::span::Span;
use std::borrow::Cow;
use std::fmt;

/// Parsing diagnostic.
#[derive(Debug, Clone, JsError)]
#[class(syntax)]
pub struct ParseDiagnostic(pub(crate) Box<ParseDiagnosticInner>);

#[derive(Debug, Clone)]
pub(crate) struct ParseDiagnosticInner {
  pub specifier: ModuleSpecifier,
  pub span: Span,
  pub message: String,
  pub source: SourceTextInfo,
}

impl Eq for ParseDiagnostic {}

impl PartialEq for ParseDiagnostic {
  fn eq(&self, other: &Self) -> bool {
    self.0.specifier == other.0.specifier
      && self.0.span == other.0.span
      && self.0.message == other.0.message
  }
}

impl ParseDiagnostic {
  /// Specifier of the source the diagnostic occurred in.
  pub fn specifier(&self) -> &ModuleSpecifier {
    &self.0.specifier
  }

  /// Span of the diagnostic.
  pub fn span(&self) -> Span {
    self.0.span
  }

  /// The diagnostic message.
  pub fn message(&self) -> &str {
    &self.0.message
  }

  pub(crate) fn source(&self) -> &SourceTextInfo {
    &self.0.source
  }

  /// Whether this diagnostic is considered fatal for transpilation.
  pub fn is_fatal(&self) -> bool {
    // All parse errors are considered fatal
    true
  }

  /// 1-indexed display position the diagnostic occurred at.
  pub fn display_position(&self) -> LineAndColumnDisplay {
    self.0.source.line_and_column_display(self.0.span.start as usize)
  }

  pub fn from_oxc_diagnostic(
    err: OxcDiagnostic,
    specifier: &ModuleSpecifier,
    source: SourceTextInfo,
  ) -> ParseDiagnostic {
    let span = err
      .labels
      .as_ref()
      .and_then(|labels| labels.first())
      .map(|label| {
        Span::new(
          label.offset() as u32,
          (label.offset() + label.len()) as u32,
        )
      })
      .unwrap_or(Span::new(0, 0));

    ParseDiagnostic(Box::new(ParseDiagnosticInner {
      specifier: specifier.clone(),
      span,
      message: err.message.to_string(),
      source,
    }))
  }

  pub fn from_message(
    message: String,
    specifier: &ModuleSpecifier,
    source: SourceTextInfo,
    span: Span,
  ) -> ParseDiagnostic {
    ParseDiagnostic(Box::new(ParseDiagnosticInner {
      specifier: specifier.clone(),
      span,
      message,
      source,
    }))
  }
}

impl Diagnostic for ParseDiagnostic {
  fn level(&self) -> DiagnosticLevel {
    DiagnosticLevel::Error
  }

  fn code(&self) -> Cow<'_, str> {
    Cow::Borrowed("parse-error")
  }

  fn message(&self) -> Cow<'_, str> {
    Cow::Borrowed(&self.0.message)
  }

  fn location(&self) -> DiagnosticLocation<'_> {
    DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(self.specifier()),
      source_pos: DiagnosticSourcePos::ByteIndex(self.0.span.start as usize),
      text_info: Cow::Borrowed(self.source()),
    }
  }

  fn snippet(&self) -> Option<DiagnosticSnippet<'_>> {
    let span = self.span();
    Some(DiagnosticSnippet {
      source: Cow::Borrowed(self.source()),
      highlights: vec![DiagnosticSnippetHighlight {
        style: DiagnosticSnippetHighlightStyle::Error,
        range: DiagnosticSourceRange {
          start: DiagnosticSourcePos::ByteIndex(span.start as usize),
          end: DiagnosticSourcePos::ByteIndex(span.end as usize),
        },
        description: None,
      }],
    })
  }

  fn hint(&self) -> Option<Cow<'_, str>> {
    None
  }

  fn snippet_fixed(&self) -> Option<DiagnosticSnippet<'_>> {
    None
  }

  fn info(&self) -> Cow<'_, [Cow<'_, str>]> {
    Cow::Borrowed(&[])
  }

  fn docs_url(&self) -> Option<Cow<'_, str>> {
    None
  }
}

impl std::error::Error for ParseDiagnostic {}

impl fmt::Display for ParseDiagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let display_position = self.display_position();
    write!(
      f,
      "{} at {}:{}:{}",
      self.0.message,
      self.specifier(),
      display_position.line_number,
      display_position.column_number,
    )
  }
}

#[derive(Debug, JsError)]
#[class(syntax)]
pub struct ParseDiagnosticsError(pub Vec<ParseDiagnostic>);

impl std::error::Error for ParseDiagnosticsError {}

impl fmt::Display for ParseDiagnosticsError {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, diagnostic) in self.0.iter().enumerate() {
      if i > 0 {
        write!(f, "\n\n")?;
      }
      write!(f, "{}", diagnostic)?
    }
    Ok(())
  }
}
