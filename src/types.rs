// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::fmt;

use crate::swc::common::Spanned;
use crate::swc::parser::error::SyntaxError;
use crate::SourceTextInfo;

/// A 0-indexed line and column type.
pub type LineAndColumnIndex = text_lines::LineAndColumnIndex;

/// A 1-indexed line and column type which should be used for
/// display purposes only (ex. in diagnostics).
pub type LineAndColumnDisplay = text_lines::LineAndColumnDisplay;

/// Parsing diagnostic.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
  /// Specifier of the source the diagnostic occurred in.
  pub specifier: String,
  /// 1-indexed display position the diagnostic occurred at.
  pub display_position: LineAndColumnDisplay,
  /// Swc syntax error
  pub kind: SyntaxError,
}

impl Diagnostic {
  /// Message text of the diagnostic.
  pub fn message(&self) -> Cow<str> {
    self.kind.msg()
  }
}

impl Diagnostic {
  pub(crate) fn from_swc_error(
    err: crate::swc::parser::error::Error,
    specifier: &str,
    source: &SourceTextInfo,
  ) -> Diagnostic {
    Diagnostic {
      display_position: source.line_and_column_display(err.span().lo),
      specifier: specifier.to_string(),
      kind: err.into_kind(),
    }
  }
}

impl std::error::Error for Diagnostic {}

impl fmt::Display for Diagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} at {}:{}:{}",
      self.message(),
      self.specifier,
      self.display_position.line_number,
      self.display_position.column_number
    )
  }
}
