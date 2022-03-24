// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

use std::cmp::Ordering;
use std::ops::Range;

use crate::swc::common::BytePos;
use crate::swc::common::Span;

#[derive(Clone, Debug)]
pub struct TextChange {
  /// Range start to end byte index.
  pub range: Range<usize>,
  /// New text to insert or replace at the provided range.
  pub new_text: String,
}

impl TextChange {
  pub fn new(start: usize, end: usize, new_text: String) -> Self {
    Self {
      range: start..end,
      new_text,
    }
  }

  pub fn from_span_and_text(span: Span, new_text: String) -> Self {
    TextChange::new(span.lo.0 as usize, span.hi.0 as usize, new_text)
  }

  /// Gets an swc span for the provided text change.
  pub fn as_span(&self) -> Span {
    Span::new(
      BytePos(self.range.start as u32),
      BytePos(self.range.end as u32),
      Default::default(),
    )
  }
}

/// Applies the text changes to the given source text.
pub fn apply_text_changes(
  source: &str,
  mut changes: Vec<TextChange>,
) -> String {
  changes.sort_by(|a, b| match a.range.start.cmp(&b.range.start) {
    Ordering::Equal => a.range.end.cmp(&b.range.end),
    ordering => ordering,
  });

  let mut last_index = 0;
  let mut final_text = String::new();

  for change in changes {
    if change.range.start > change.range.end {
      panic!(
        "Text change had start index {} greater than end index {}.",
        change.range.start, change.range.end
      )
    }
    if change.range.start < last_index {
      panic!("Text changes were overlapping. Past index was {}, but new change had index {}.", last_index, change.range.start);
    } else if change.range.start > last_index && last_index < source.len() {
      final_text.push_str(
        &source[last_index..std::cmp::min(source.len(), change.range.start)],
      );
    }
    final_text.push_str(&change.new_text);
    last_index = change.range.end;
  }

  if last_index < source.len() {
    final_text.push_str(&source[last_index..]);
  }

  final_text
}

#[cfg(test)]
mod test {
  use super::*;

  #[test]
  fn applies_text_changes() {
    // replacing text
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![
          TextChange::new(9, 10, "z".to_string()),
          TextChange::new(4, 6, "y".to_string()),
          TextChange::new(1, 2, "x".to_string()),
        ]
      ),
      "0x23y678z".to_string(),
    );

    // replacing beside
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![
          TextChange::new(0, 5, "a".to_string()),
          TextChange::new(5, 7, "b".to_string()),
          TextChange::new(7, 10, "c".to_string()),
        ]
      ),
      "abc".to_string(),
    );

    // full replace
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(0, 10, "x".to_string()),]
      ),
      "x".to_string(),
    );

    // 1 over
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(0, 11, "x".to_string()),]
      ),
      "x".to_string(),
    );

    // insert
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(5, 5, "x".to_string()),]
      ),
      "01234x56789".to_string(),
    );

    // prepend
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(0, 0, "x".to_string()),]
      ),
      "x0123456789".to_string(),
    );

    // append
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(10, 10, "x".to_string()),]
      ),
      "0123456789x".to_string(),
    );

    // append over
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![TextChange::new(11, 11, "x".to_string()),]
      ),
      "0123456789x".to_string(),
    );

    // multiple at start
    assert_eq!(
      apply_text_changes(
        "0123456789",
        vec![
          TextChange::new(0, 7, "a".to_string()),
          TextChange::new(0, 0, "b".to_string()),
          TextChange::new(0, 0, "c".to_string()),
          TextChange::new(7, 10, "d".to_string()),
        ]
      ),
      "bcad".to_string(),
    );
  }

  #[test]
  #[should_panic(
    expected = "Text changes were overlapping. Past index was 10, but new change had index 5."
  )]
  fn panics_text_change_within() {
    apply_text_changes(
      "0123456789",
      vec![
        TextChange::new(3, 10, "x".to_string()),
        TextChange::new(5, 7, "x".to_string()),
      ],
    );
  }

  #[test]
  #[should_panic(
    expected = "Text changes were overlapping. Past index was 4, but new change had index 3."
  )]
  fn panics_text_change_overlap() {
    apply_text_changes(
      "0123456789",
      vec![
        TextChange::new(2, 4, "x".to_string()),
        TextChange::new(3, 5, "x".to_string()),
      ],
    );
  }

  #[test]
  #[should_panic(
    expected = "Text change had start index 2 greater than end index 1."
  )]
  fn panics_start_greater_end() {
    apply_text_changes(
      "0123456789",
      vec![TextChange::new(2, 1, "x".to_string())],
    );
  }
}
