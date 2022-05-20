// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::fmt;

use crate::swc::parser::error::SyntaxError;
use crate::LineAndColumnDisplay;
use crate::SourceRange;
use crate::SourceRangedForSpanned;
use crate::SourceTextInfo;

/// Parsing diagnostic.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
  /// Specifier of the source the diagnostic occurred in.
  pub specifier: String,
  /// Range of the diagnostic.
  pub range: SourceRange,
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
      range: err.range(),
      display_position: source.line_and_column_display(err.range().start),
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

#[derive(Debug)]
pub struct DiagnosticsError(pub Vec<Diagnostic>);

impl std::error::Error for DiagnosticsError {}

impl fmt::Display for DiagnosticsError {
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
