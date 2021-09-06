// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::sync::Arc;
use text_lines::TextLines;

use super::types::LineAndColumnDisplay;
use super::types::LineAndColumnIndex;
use crate::swc::common::BytePos;
use crate::swc::common::Span;

#[derive(Clone)]
pub struct SourceTextInfo {
  start_pos: BytePos,
  text: Arc<String>,
  text_lines: Arc<TextLines>,
}

impl SourceTextInfo {
  pub fn new(start_pos: BytePos, text: Arc<String>) -> Self {
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

    Self::with_indent_width(start_pos, text, 2)
  }

  pub fn from_string(mut text: String) -> Self {
    strip_bom_mut(&mut text);
    Self::new(BytePos(0), Arc::new(text))
  }

  pub fn with_indent_width(
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

  pub fn text(&self) -> Arc<String> {
    self.text.clone()
  }

  pub fn text_str(&self) -> &str {
    self.text.as_str()
  }

  pub fn span(&self) -> Span {
    let start = self.start_pos;
    Span::new(
      start,
      start + BytePos(self.text.len() as u32),
      Default::default(),
    )
  }

  pub fn lines_count(&self) -> usize {
    self.text_lines.lines_count()
  }

  pub fn line_index(&self, pos: BytePos) -> usize {
    self.assert_pos(pos);
    self
      .text_lines
      .line_index(self.get_relative_index_from_pos(pos))
  }

  pub fn line_start(&self, line_index: usize) -> BytePos {
    self.assert_line_index(line_index);
    self.get_pos_from_relative_index(self.text_lines.line_start(line_index))
  }

  pub fn line_end(&self, line_index: usize) -> BytePos {
    self.assert_line_index(line_index);
    self.get_pos_from_relative_index(self.text_lines.line_end(line_index))
  }

  pub fn line_and_column_index(&self, pos: BytePos) -> LineAndColumnIndex {
    self.assert_pos(pos);
    self
      .text_lines
      .line_and_column_index(self.get_relative_index_from_pos(pos))
  }

  pub fn line_and_column_display(&self, pos: BytePos) -> LineAndColumnDisplay {
    self.assert_pos(pos);
    self
      .text_lines
      .line_and_column_display(self.get_relative_index_from_pos(pos))
  }

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

  pub fn line_text(&self, line_index: usize) -> &str {
    let line_start = self.line_start(line_index).0 as usize;
    let line_end = self.line_end(line_index).0 as usize;
    &self.text_str()[line_start..line_end]
  }

  /// Gets the source text located within the specified span.
  pub fn get_span_text(&self, span: &Span) -> &str {
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
        "The specified line index {} was greater or equal to the number of lines of {}.",
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

const BOM_CHAR: char = '\u{FEFF}';

/// Strips the byte order mark if it exists from the provided text in place.
fn strip_bom_mut(text: &mut String) {
  if text.starts_with(BOM_CHAR) {
    text.drain(..BOM_CHAR.len_utf8());
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
        SourceTextInfo::new(BytePos(i), Arc::new(text.to_string()));
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
    let info = SourceTextInfo::new(BytePos(1), Arc::new("test".to_string()));
    info.line_and_column_index(BytePos(0));
  }

  #[test]
  #[should_panic(
    expected = "The provided position 6 was greater than the end position 5."
  )]
  fn line_and_column_index_panic_greater_than() {
    let info = SourceTextInfo::new(BytePos(1), Arc::new("test".to_string()));
    info.line_and_column_index(BytePos(6));
  }

  #[test]
  fn line_start() {
    let text = "12\n3\r\n4\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new(BytePos(i), Arc::new(text.to_string()));
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
    expected = "The specified line index 1 was greater or equal to the number of lines of 1."
  )]
  fn line_start_equal_number_lines() {
    let info = SourceTextInfo::new(BytePos(1), Arc::new("test".to_string()));
    info.line_start(1);
  }

  #[test]
  fn line_end() {
    let text = "12\n3\r\n4\n5";
    for i in 0..10 {
      let source_file =
        SourceTextInfo::new(BytePos(i), Arc::new(text.to_string()));
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
    expected = "The specified line index 1 was greater or equal to the number of lines of 1."
  )]
  fn line_end_equal_number_lines() {
    let info = SourceTextInfo::new(BytePos(1), Arc::new("test".to_string()));
    info.line_end(1);
  }

  #[test]
  fn strip_bom_mut_with_bom() {
    let mut text = format!("{}text", BOM_CHAR);
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }

  #[test]
  fn strip_bom_mut_without_bom() {
    let mut text = "text".to_string();
    strip_bom_mut(&mut text);
    assert_eq!(text, "text");
  }
}
