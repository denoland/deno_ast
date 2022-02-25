// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;
use text_lines::TextLines;

use super::types::LineAndColumnDisplay;
use super::types::LineAndColumnIndex;
use crate::swc::common::BytePos;
use crate::swc::common::Span;
use crate::text_encoding::strip_bom_mut;
use crate::text_encoding::BOM_CHAR;

/// Stores the source text along with other data such as where all the lines
/// occur in the text.
///
/// Note: This struct is cheap to clone.
#[derive(Clone)]
pub struct SourceTextInfo {
  // keep this struct cheap to clone
  start_pos: BytePos,
  text: Arc<String>,
  text_lines: Arc<TextLines>,
}

impl SourceTextInfo {
  /// Creates a new `SourceTextInfo` from the provided source text.
  pub fn new(text: Arc<String>) -> Self {
    Self::new_with_pos(BytePos(0), text)
  }

  /// Creates a new `SourceTextInfo` from the provided byte position
  /// and source text.
  ///
  /// Generally, most files will have a start position of `BytePos(0)`
  /// and when in doubt provide that, but SWC will not necessarily
  /// start files with `BytePos(0)` when bundling.
  pub fn new_with_pos(start_pos: BytePos, text: Arc<String>) -> Self {
    // The BOM should be stripped before it gets passed here
    // because it's a text encoding concern that should be
    // stripped when the file is read.
    // todo(dsherret): re-enable once stripped in deno_graph
    // debug_assert!(!text.starts_with(BOM_CHAR));

    let text = if text.starts_with(BOM_CHAR) {
      let mut text = match Arc::try_unwrap(text) {
        Ok(text) => text,
        Err(text) => text.to_string(),
      };
      strip_bom_mut(&mut text);
      Arc::new(text)
    } else {
      text
    };

    Self::new_with_indent_width(start_pos, text, 2)
  }

  /// Creates a new `SourceTextInfo` from the provided start position,
  /// source text, and indentation width.
  ///
  /// The indentation width determines the number of columns to use
  /// when going over a tab character. For example, an indent width
  /// of 2 will mean each tab character will represent 2 columns.
  /// The default indentation width used in the other methods is `2`
  /// to match the default indentation used by `deno fmt`.
  pub fn new_with_indent_width(
    start_pos: BytePos,
    text: Arc<String>,
    indent_width: usize,
  ) -> Self {
    Self {
      start_pos,
      text_lines: Arc::new(TextLines::with_indent_width(&text, indent_width)),
      text,
    }
  }

  /// Creates a new `SourceTextInfo` from the provided source text.
  ///
  /// Generally, prefer using `SourceTextInfo::new` to provide a
  /// string already in an `std::sync::Arc`.
  pub fn from_string(mut text: String) -> Self {
    strip_bom_mut(&mut text);
    Self::new(Arc::new(text))
  }

  /// Gets the source text.
  pub fn text(&self) -> Arc<String> {
    self.text.clone()
  }

  /// Gets a reference to the source text.
  pub fn text_str(&self) -> &str {
    self.text.as_str()
  }

  /// Gets the range—start and end byte position—of the source text.
  pub fn span(&self) -> Span {
    let start = self.start_pos;
    Span::new(
      start,
      start + BytePos(self.text.len() as u32),
      Default::default(),
    )
  }

  /// Gets the number of lines in the source text.
  pub fn lines_count(&self) -> usize {
    self.text_lines.lines_count()
  }

  /// Gets the 0-indexed line index at the provided byte position.
  ///
  /// Note that this will panic when providing a byte position outside
  /// the range of the source text.
  pub fn line_index(&self, pos: BytePos) -> usize {
    self.assert_pos(pos);
    self
      .text_lines
      .line_index(self.get_relative_index_from_pos(pos))
  }

  /// Gets the line start byte position of the provided 0-indexed line index.
  ///
  /// Note that this will panic if providing a line index outside the
  /// bounds of the number of lines.
  pub fn line_start(&self, line_index: usize) -> BytePos {
    self.assert_line_index(line_index);
    self.get_pos_from_relative_index(self.text_lines.line_start(line_index))
  }

  /// Gets the line end byte position of the provided 0-indexed line index.
  ///
  /// Note that this will panic if providing a line index outside the
  /// bounds of the number of lines.
  pub fn line_end(&self, line_index: usize) -> BytePos {
    self.assert_line_index(line_index);
    self.get_pos_from_relative_index(self.text_lines.line_end(line_index))
  }

  /// Gets the 0-indexed line and column index of the provided byte position.
  ///
  /// Note that this will panic when providing a byte position outside
  /// the range of the source text.
  pub fn line_and_column_index(&self, pos: BytePos) -> LineAndColumnIndex {
    self.assert_pos(pos);
    self
      .text_lines
      .line_and_column_index(self.get_relative_index_from_pos(pos))
  }

  /// Gets the 1-indexed line and column index of the provided byte position
  /// taking into account the default indentation width.
  ///
  /// Note that this will panic when providing a byte position outside
  /// the range of the source text.
  pub fn line_and_column_display(&self, pos: BytePos) -> LineAndColumnDisplay {
    self.assert_pos(pos);
    self
      .text_lines
      .line_and_column_display(self.get_relative_index_from_pos(pos))
  }

  /// Gets the 1-indexed line and column index of the provided byte position
  /// with a custom indentation width.
  ///
  /// Note that this will panic when providing a byte position outside
  /// the range of the source text.
  pub fn line_and_column_display_with_indent_width(
    &self,
    pos: BytePos,
    indent_width: usize,
  ) -> LineAndColumnDisplay {
    self.assert_pos(pos);
    self.text_lines.line_and_column_display_with_indent_width(
      self.get_relative_index_from_pos(pos),
      indent_width,
    )
  }

  /// Gets the byte position of the provided line and column index.
  ///
  /// Note that this will panic if providing a line index outside the
  /// bounds of the number of lines, but will clip the the line end byte index
  /// when exceeding the line length.
  pub fn byte_index(
    &self,
    line_and_column_index: LineAndColumnIndex,
  ) -> BytePos {
    self.assert_line_index(line_and_column_index.line_index);
    self.get_pos_from_relative_index(
      self.text_lines.byte_index(line_and_column_index),
    )
  }

  /// Gets a reference to the text slice of the line at the provided
  /// 0-based index.
  ///
  /// Note that this will panic if providing a line index outside the
  /// bounds of the number of lines.
  pub fn line_text(&self, line_index: usize) -> &str {
    let line_start = self.line_start(line_index).0 as usize;
    let line_end = self.line_end(line_index).0 as usize;
    &self.text_str()[line_start..line_end]
  }

  /// Gets the source text located within the provided span.
  pub fn span_text(&self, span: &Span) -> &str {
    let offset_lo = (span.lo() - self.start_pos).0 as usize;
    let offset_hi = (span.hi - self.start_pos).0 as usize;
    &self.text_str()[offset_lo..offset_hi]
  }

  fn assert_pos(&self, pos: BytePos) {
    let span = self.span();
    if pos < span.lo() {
      panic!(
        "The provided position {} was less than the start position {}.",
        pos.0,
        span.lo().0
      );
    } else if pos > span.hi() {
      panic!(
        "The provided position {} was greater than the end position {}.",
        pos.0,
        span.hi().0
      );
    }
  }

  fn assert_line_index(&self, line_index: usize) {
    if line_index >= self.lines_count() {
      panic!(
        "The provided line index {} was greater or equal to the number of lines of {}.",
        line_index,
        self.lines_count()
      );
    }
  }

  fn get_relative_index_from_pos(&self, pos: BytePos) -> usize {
    (pos - self.start_pos).0 as usize
  }

  fn get_pos_from_relative_index(&self, relative_index: usize) -> BytePos {
    self.start_pos + BytePos(relative_index as u32)
  }
}

impl std::fmt::Debug for SourceTextInfo {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    f.debug_struct("ParsedSourceTextInfo")
      .field("start_pos", &self.start_pos)
      .field("text", &self.text)
      .finish()
  }
}

#[cfg(feature = "view")]
impl crate::view::SourceFile for SourceTextInfo {
  fn text(&self) -> &str {
    SourceTextInfo::text_str(self)
  }

  fn span(&self) -> Span {
    SourceTextInfo::span(self)
  }

  fn lines_count(&self) -> usize {
    SourceTextInfo::lines_count(self)
  }

  fn line_index(&self, pos: BytePos) -> usize {
    SourceTextInfo::line_index(self, pos)
  }

  fn line_start(&self, line_index: usize) -> BytePos {
    SourceTextInfo::line_start(self, line_index)
  }

  fn line_end(&self, line_index: usize) -> BytePos {
    SourceTextInfo::line_end(self, line_index)
  }

  fn line_and_column_index(&self, pos: BytePos) -> LineAndColumnIndex {
    SourceTextInfo::line_and_column_index(self, pos)
  }
}

#[cfg(test)]
mod test {
  use super::*;

  use swc_common::BytePos;

  #[test]
  fn line_and_column_index() {
    let text = "12\n3\r\nβ\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new_with_pos(BytePos(i), Arc::new(text.to_string()));
      assert_pos_line_and_col(&source_file, i, 0, 0); // 1
      assert_pos_line_and_col(&source_file, 1 + i, 0, 1); // 2
      assert_pos_line_and_col(&source_file, 2 + i, 0, 2); // \n
      assert_pos_line_and_col(&source_file, 3 + i, 1, 0); // 3
      assert_pos_line_and_col(&source_file, 4 + i, 1, 1); // \r
      assert_pos_line_and_col(&source_file, 5 + i, 1, 2); // \n
      assert_pos_line_and_col(&source_file, 6 + i, 2, 0); // first β index
      assert_pos_line_and_col(&source_file, 7 + i, 2, 0); // second β index
      assert_pos_line_and_col(&source_file, 8 + i, 2, 1); // \n
      assert_pos_line_and_col(&source_file, 9 + i, 3, 0); // 5
      assert_pos_line_and_col(&source_file, 10 + i, 3, 1); // <EOF>
    }
  }

  fn assert_pos_line_and_col(
    source_file: &SourceTextInfo,
    pos: u32,
    line_index: usize,
    column_index: usize,
  ) {
    assert_eq!(
      source_file.line_and_column_index(BytePos(pos)),
      LineAndColumnIndex {
        line_index,
        column_index,
      }
    );
  }

  #[test]
  #[should_panic(
    expected = "The provided position 0 was less than the start position 1."
  )]
  fn line_and_column_index_panic_less_than() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("test".to_string()));
    info.line_and_column_index(BytePos(0));
  }

  #[test]
  #[should_panic(
    expected = "The provided position 6 was greater than the end position 5."
  )]
  fn line_and_column_index_panic_greater_than() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("test".to_string()));
    info.line_and_column_index(BytePos(6));
  }

  #[test]
  fn byte_index() {
    let text = "12\n3\r\nβ\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new_with_pos(BytePos(i), Arc::new(text.to_string()));
      assert_byte_index(&source_file, 0, 0, i); // 1
      assert_byte_index(&source_file, 0, 1, 1 + i); // 2
      assert_byte_index(&source_file, 0, 2, 2 + i); // \n
      assert_byte_index(&source_file, 0, 3, 2 + i); // beyond newline
      assert_byte_index(&source_file, 0, 4, 2 + i); // beyond newline
      assert_byte_index(&source_file, 1, 0, 3 + i); // 3
      assert_byte_index(&source_file, 1, 1, 4 + i); // \r
      assert_byte_index(&source_file, 1, 2, 4 + i); // \n
      assert_byte_index(&source_file, 2, 0, 6 + i); // β
      assert_byte_index(&source_file, 2, 1, 8 + i); // \n
      assert_byte_index(&source_file, 3, 0, 9 + i); // 5
      assert_byte_index(&source_file, 3, 1, 10 + i); // <EOF>
      assert_byte_index(&source_file, 3, 2, 10 + i); // beyond <EOF>
    }
  }

  fn assert_byte_index(
    source_file: &SourceTextInfo,
    line_index: usize,
    column_index: usize,
    pos: u32,
  ) {
    assert_eq!(
      source_file.byte_index(LineAndColumnIndex {
        line_index,
        column_index,
      }),
      BytePos(pos)
    );
  }

  #[test]
  #[should_panic(
    expected = "The provided line index 1 was greater or equal to the number of lines of 1."
  )]
  fn byte_index_panic_greater_than_lines() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("test".to_string()));
    info.byte_index(LineAndColumnIndex {
      line_index: 1,
      column_index: 0,
    });
  }

  #[test]
  fn line_start() {
    let text = "12\n3\r\n4\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new_with_pos(BytePos(i), Arc::new(text.to_string()));
      assert_line_start(&source_file, 0, BytePos(i));
      assert_line_start(&source_file, 1, BytePos(3 + i));
      assert_line_start(&source_file, 2, BytePos(6 + i));
      assert_line_start(&source_file, 3, BytePos(8 + i));
    }
  }

  fn assert_line_start(
    source_file: &SourceTextInfo,
    line_index: usize,
    line_end: BytePos,
  ) {
    assert_eq!(source_file.line_start(line_index), line_end,);
  }

  #[test]
  #[should_panic(
    expected = "The provided line index 1 was greater or equal to the number of lines of 1."
  )]
  fn line_start_equal_number_lines() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("test".to_string()));
    info.line_start(1);
  }

  #[test]
  fn line_end() {
    let text = "12\n3\r\n4\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new_with_pos(BytePos(i), Arc::new(text.to_string()));
      assert_line_end(&source_file, 0, BytePos(2 + i));
      assert_line_end(&source_file, 1, BytePos(4 + i));
      assert_line_end(&source_file, 2, BytePos(7 + i));
      assert_line_end(&source_file, 3, BytePos(9 + i));
    }
  }

  fn assert_line_end(
    source_file: &SourceTextInfo,
    line_index: usize,
    line_end: BytePos,
  ) {
    assert_eq!(source_file.line_end(line_index), line_end);
  }

  #[test]
  #[should_panic(
    expected = "The provided line index 1 was greater or equal to the number of lines of 1."
  )]
  fn line_end_equal_number_lines() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("test".to_string()));
    info.line_end(1);
  }

  #[test]
  fn span_text() {
    let info =
      SourceTextInfo::new_with_pos(BytePos(1), Arc::new("abcd".to_string()));
    assert_eq!(
      info.span_text(&Span::new(BytePos(1), BytePos(2), Default::default())),
      "a"
    )
  }
}
