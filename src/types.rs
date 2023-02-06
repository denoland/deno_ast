// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::fmt;

use crate::swc::parser::error::SyntaxError;
use crate::LineAndColumnDisplay;
use crate::SourceRange;
use crate::SourceRangedForSpanned;
use crate::SourceTextInfo;

/// Parsing diagnostic.
#[derive(Debug, Clone)]
pub struct Diagnostic {
  /// Specifier of the source the diagnostic occurred in.
  pub specifier: String,
  /// Range of the diagnostic.
  pub range: SourceRange,
  /// Swc syntax error
  pub kind: SyntaxError,
  source: SourceTextInfo,
}

impl PartialEq for Diagnostic {
  fn eq(&self, other: &Self) -> bool {
    // excludes the source
    self.specifier == other.specifier
      && self.range == other.range
      && self.kind == other.kind
  }
}

impl Diagnostic {
  /// Message text of the diagnostic.
  pub fn message(&self) -> Cow<str> {
    self.kind.msg()
  }

  /// 1-indexed display position the diagnostic occurred at.
  pub fn display_position(&self) -> LineAndColumnDisplay {
    self.source.line_and_column_display(self.range.start)
  }
}

impl Diagnostic {
  pub fn from_swc_error(
    err: crate::swc::parser::error::Error,
    specifier: &str,
    source: SourceTextInfo,
  ) -> Diagnostic {
    Diagnostic {
      range: err.range(),
      specifier: specifier.to_string(),
      kind: err.into_kind(),
      source,
    }
  }
}

impl std::error::Error for Diagnostic {}

impl fmt::Display for Diagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let display_position = self.display_position();
    write!(
      f,
      "{} at {}:{}:{}\n\n{}",
      self.message(),
      self.specifier,
      display_position.line_number,
      display_position.column_number,
      // todo(dsherret): remove this catch unwind once we've
      // tested this out a lot
      std::panic::catch_unwind(|| {
        get_range_text_highlight(&self.source, self.range)
          .lines()
          // indent two spaces
          .map(|l| {
            if l.trim().is_empty() {
              String::new()
            } else {
              format!("  {}", l)
            }
          })
          .collect::<Vec<_>>()
          .join("\n")
      })
      .unwrap_or_else(|err| {
        format!("Bug. Please report this issue: {:?}", err)
      }),
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

/// Code in this function was adapted from:
/// https://github.com/dprint/dprint/blob/a026a1350d27a61ea18207cb31897b18eaab51a1/crates/core/src/formatting/utils/string_utils.rs#L62
fn get_range_text_highlight(
  source: &SourceTextInfo,
  byte_range: SourceRange,
) -> String {
  fn get_column_index_of_pos(text: &str, pos: usize) -> usize {
    let line_start_byte_pos = get_line_start_byte_pos(text, pos);
    text[line_start_byte_pos..pos].chars().count()
  }

  fn get_line_start_byte_pos(text: &str, pos: usize) -> usize {
    let text_bytes = text.as_bytes();
    for i in (0..pos).rev() {
      if text_bytes.get(i) == Some(&(b'\n')) {
        return i + 1;
      }
    }

    0
  }

  fn get_text_and_error_range(
    source: &SourceTextInfo,
    byte_range: SourceRange,
  ) -> (&str, (usize, usize)) {
    let first_line_start =
      source.line_start(source.line_index(byte_range.start));
    let last_line_end = source.line_end(source.line_index(byte_range.end));

    let error_start = byte_range.start - first_line_start;
    let error_end = error_start + (byte_range.end - byte_range.start);
    let sub_text =
      source.range_text(&SourceRange::new(first_line_start, last_line_end));
    (sub_text, (error_start, error_end))
  }

  let (sub_text, (error_start, error_end)) =
    get_text_and_error_range(source, byte_range);

  let mut result = String::new();
  // don't use .lines() here because it will trim any empty
  // lines, which might for some reason be part of the range
  let lines = sub_text.split('\n').collect::<Vec<_>>();
  let line_count = lines.len();
  for (i, mut line) in lines.into_iter().enumerate() {
    if line.ends_with('\r') {
      line = &line[..line.len() - 1]; // trim the \r
    }
    let is_last_line = i == line_count - 1;
    // don't show all the lines if there are more than 3 lines
    if i > 2 && !is_last_line {
      continue;
    }
    if i > 0 {
      result.push('\n');
    }
    if i == 2 && !is_last_line {
      result.push_str("...");
      continue;
    }

    let mut error_start_char_index = if i == 0 {
      get_column_index_of_pos(sub_text, error_start)
    } else {
      0
    };
    let mut error_end_char_index = if is_last_line {
      get_column_index_of_pos(sub_text, error_end)
    } else {
      line.chars().count()
    };
    let line_char_count = line.chars().count();
    if line_char_count > 90 {
      let start_char_index = if error_start_char_index > 60 {
        std::cmp::min(error_start_char_index - 20, line_char_count - 80)
      } else {
        0
      };
      error_start_char_index -= start_char_index;
      error_end_char_index -= start_char_index;
      let code_text = line
        .chars()
        .skip(start_char_index)
        .take(80)
        .collect::<String>();
      let mut line_text = String::new();
      if start_char_index > 0 {
        line_text.push_str("...");
        error_start_char_index += 3;
        error_end_char_index += 3;
      }
      line_text.push_str(&code_text);
      if line_char_count > start_char_index + code_text.chars().count() {
        error_end_char_index =
          std::cmp::min(error_end_char_index, line_text.chars().count());
        line_text.push_str("...");
      }
      result.push_str(&line_text);
    } else {
      result.push_str(line);
    }
    result.push('\n');

    result.push_str(&" ".repeat(error_start_char_index));
    result.push_str(&"~".repeat(std::cmp::max(
      1, // this means it's the end of the line, so display a single ~
      error_end_char_index - error_start_char_index,
    )));
  }
  result
}

#[cfg(test)]
mod test {
  use dprint_swc_ext::common::SourceRange;
  use dprint_swc_ext::common::SourceTextInfo;
  use pretty_assertions::assert_eq;

  use super::get_range_text_highlight;

  #[test]
  fn range_highlight_all_text() {
    let text = SourceTextInfo::from_string(
      concat!(
        "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6\n",
        "Line 1\n",
        "Line 2\n",
        "Line 3\n",
        "Line 4"
      ).to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_end(4))
      ),
      concat!(
        "Line 0 - Testing this out with a long line testing0 testing1 testing2 testing3 t...\n",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n",
        "Line 1\n",
        "~~~~~~\n",
        "...\n",
        "Line 4\n",
        "~~~~~~",
      ),
    );
  }

  #[test]
  fn range_highlight_all_text_last_line_long() {
    let text = SourceTextInfo::from_string(
      concat!(
        "Line 0\n",
        "Line 1\n",
        "Line 2\n",
        "Line 3\n",
        "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6\n",
      ).to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_end(4))
      ),
      concat!(
        "Line 0\n",
        "~~~~~~\n",
        "Line 1\n",
        "~~~~~~\n",
        "...\n",
        "Line 4 - Testing this out with a long line testing0 testing1 testing2 testing3 t...\n",
        "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~",
      ),
    );
  }

  #[test]
  fn range_highlight_range_start_long_line() {
    let text = SourceTextInfo::from_string(
      "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7".to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_start(0), text.line_start(0) + 1)
      ),
      concat!(
        "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 t...\n",
        "~",
      ),
    );
  }

  #[test]
  fn range_highlight_range_end_long_line() {
    let text = SourceTextInfo::from_string(
      "Testing this out with a long line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7".to_string(),
    );
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0) - 1, text.line_end(0))
      ),
      concat!(
        "...ong line testing0 testing1 testing2 testing3 testing4 testing5 testing6 testing7\n",
        "                                                                                  ~",
      ),
    );
  }

  #[test]
  fn range_highlight_whitespace_start_line() {
    let text = SourceTextInfo::from_string("  testing\r\ntest".to_string());
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0) - 1, text.line_end(1))
      ),
      concat!("  testing\n", "        ~\n", "test\n", "~~~~",),
    );
  }

  #[test]
  fn range_end_of_line() {
    let text =
      SourceTextInfo::from_string("  testingtestingtestingtesting".to_string());
    assert_eq!(
      get_range_text_highlight(
        &text,
        SourceRange::new(text.line_end(0), text.line_end(0))
      ),
      concat!(
        "  testingtestingtestingtesting\n",
        "                              ~",
      ),
    );
  }
}
