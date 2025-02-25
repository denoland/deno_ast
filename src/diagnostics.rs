// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::fmt::Write as _;
use std::path::PathBuf;

use deno_terminal::colors;
use unicode_width::UnicodeWidthStr;

use crate::ModuleSpecifier;
use crate::SourcePos;
use crate::SourceRange;
use crate::SourceRanged;
use crate::SourceTextInfo;

pub enum DiagnosticLevel {
  Error,
  Warning,
}

#[derive(Clone, Copy, Debug)]
pub struct DiagnosticSourceRange {
  pub start: DiagnosticSourcePos,
  pub end: DiagnosticSourcePos,
}

#[derive(Clone, Copy, Debug)]
pub enum DiagnosticSourcePos {
  SourcePos(SourcePos),
  ByteIndex(usize),
  LineAndCol {
    // 0-indexed line number in bytes
    line: usize,
    // 0-indexed column number in bytes
    column: usize,
  },
}

impl DiagnosticSourcePos {
  fn pos(&self, source: &SourceTextInfo) -> SourcePos {
    match self {
      DiagnosticSourcePos::SourcePos(pos) => *pos,
      DiagnosticSourcePos::ByteIndex(index) => source.range().start() + *index,
      DiagnosticSourcePos::LineAndCol { line, column } => {
        source.line_start(*line) + *column
      }
    }
  }
}

#[derive(Clone, Debug)]
pub enum DiagnosticLocation<'a> {
  /// The diagnostic is relevant to a specific path.
  Path { path: PathBuf },
  /// The diagnostic is relevant to an entire module.
  Module {
    /// The specifier of the module that contains the diagnostic.
    specifier: Cow<'a, ModuleSpecifier>,
  },
  /// The diagnostic is relevant to a specific position in a module.
  ///
  /// This variant will get the relevant `SouceTextInfo` from the cache using
  /// the given specifier, and will then calculate the line and column numbers
  /// from the given `SourcePos`.
  ModulePosition {
    /// The specifier of the module that contains the diagnostic.
    specifier: Cow<'a, ModuleSpecifier>,
    /// The source position of the diagnostic.
    source_pos: DiagnosticSourcePos,
    text_info: Cow<'a, SourceTextInfo>,
  },
}

impl DiagnosticLocation<'_> {
  /// Return the line and column number of the diagnostic.
  ///
  /// The line number is 1-indexed.
  ///
  /// The column number is 1-indexed. This is the number of UTF-16 code units
  /// from the start of the line to the diagnostic.
  /// Why UTF-16 code units? Because that's what VS Code understands, and
  /// everyone uses VS Code. :)
  fn position(&self) -> Option<(usize, usize)> {
    match self {
      DiagnosticLocation::Path { .. } => None,
      DiagnosticLocation::Module { .. } => None,
      DiagnosticLocation::ModulePosition {
        specifier: _specifier,
        source_pos,
        text_info,
      } => {
        let pos = source_pos.pos(text_info);
        let line_index = text_info.line_index(pos);
        let line_start_pos = text_info.line_start(line_index);
        // todo(dsherret): fix in text_lines
        let content =
          text_info.range_text(&SourceRange::new(line_start_pos, pos));
        let line = line_index + 1;
        let column = content.encode_utf16().count() + 1;
        Some((line, column))
      }
    }
  }
}

pub struct DiagnosticSnippet<'a> {
  /// The source text for this snippet. The
  pub source: Cow<'a, crate::SourceTextInfo>,
  /// The piece of the snippet that should be highlighted. For best results, the
  /// highlights should not overlap and be ordered by their start position.
  pub highlights: Vec<DiagnosticSnippetHighlight<'a>>,
}

#[derive(Clone)]
pub struct DiagnosticSnippetHighlight<'a> {
  /// The range of the snippet that should be highlighted.
  pub range: DiagnosticSourceRange,
  /// The style of the highlight.
  pub style: DiagnosticSnippetHighlightStyle,
  /// An optional inline description of the highlight.
  pub description: Option<Cow<'a, str>>,
}

#[derive(Clone, Copy)]
pub enum DiagnosticSnippetHighlightStyle {
  /// The highlight is an error. This will place red carets under the highlight.
  Error,
  #[allow(dead_code)]
  /// The highlight is a warning. This will place yellow carets under the
  /// highlight.
  Warning,
  #[allow(dead_code)]
  /// The highlight shows a hint. This will place blue dashes under the
  /// highlight.
  Hint,
}

impl DiagnosticSnippetHighlightStyle {
  fn style_underline(
    &self,
    s: impl std::fmt::Display,
  ) -> impl std::fmt::Display {
    match self {
      DiagnosticSnippetHighlightStyle::Error => colors::red_bold(s),
      DiagnosticSnippetHighlightStyle::Warning => colors::yellow_bold(s),
      DiagnosticSnippetHighlightStyle::Hint => colors::intense_blue(s),
    }
  }

  fn underline_char(&self) -> char {
    match self {
      DiagnosticSnippetHighlightStyle::Error => '^',
      DiagnosticSnippetHighlightStyle::Warning => '^',
      DiagnosticSnippetHighlightStyle::Hint => '-',
    }
  }
}

/// Returns the text of the line with the given number.
fn line_text(source: &SourceTextInfo, line_number: usize) -> &str {
  source.line_text(line_number - 1)
}

/// Returns the line number (1 indexed) of the line that contains the given
/// position.
fn line_number(source: &SourceTextInfo, pos: DiagnosticSourcePos) -> usize {
  source.line_index(pos.pos(source)) + 1
}

pub trait Diagnostic {
  /// The level of the diagnostic.
  fn level(&self) -> DiagnosticLevel;

  /// The diagnostic code, like `no-explicit-any` or `ban-untagged-ignore`.
  fn code(&self) -> Cow<'_, str>;

  /// The human-readable diagnostic message.
  fn message(&self) -> Cow<'_, str>;

  /// The location this diagnostic is associated with.
  fn location(&self) -> DiagnosticLocation;

  /// A snippet showing the source code associated with the diagnostic.
  fn snippet(&self) -> Option<DiagnosticSnippet<'_>>;

  /// A hint for fixing the diagnostic.
  fn hint(&self) -> Option<Cow<'_, str>>;

  /// A snippet showing how the diagnostic can be fixed.
  fn snippet_fixed(&self) -> Option<DiagnosticSnippet<'_>>;

  fn info(&self) -> Cow<'_, [Cow<'_, str>]>;

  /// An optional URL to the documentation for the diagnostic.
  fn docs_url(&self) -> Option<Cow<'_, str>>;

  fn display(&self) -> DiagnosticDisplay<Self> {
    DiagnosticDisplay { diagnostic: self }
  }
}

struct RepeatingCharFmt(char, usize);
impl fmt::Display for RepeatingCharFmt {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for _ in 0..self.1 {
      f.write_char(self.0)?;
    }
    Ok(())
  }
}

/// How many spaces a tab should be displayed as. 2 is the default used for
/// `deno fmt`, so we'll use that here.
const TAB_WIDTH: usize = 2;

struct ReplaceTab<'a>(&'a str);
impl fmt::Display for ReplaceTab<'_> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut written = 0;
    for (i, c) in self.0.char_indices() {
      if c == '\t' {
        self.0[written..i].fmt(f)?;
        RepeatingCharFmt(' ', TAB_WIDTH).fmt(f)?;
        written = i + 1;
      }
    }
    self.0[written..].fmt(f)?;
    Ok(())
  }
}

/// The width of the string as displayed, assuming tabs are 2 spaces wide.
///
/// This display width assumes that zero-width-joined characters are the width
/// of their consituent characters. This means that "Person: Red Hair" (which is
/// represented as "Person" + "ZWJ" + "Red Hair") will have a width of 4.
///
/// Whether this is correct is unfortunately dependent on the font / terminal
/// being used. Here is a list of what terminals consider the length of
/// "Person: Red Hair" to be:
///
/// | Terminal         | Rendered Width |
/// | ---------------- | -------------- |
/// | Windows Terminal | 5 chars        |
/// | iTerm (macOS)    | 2 chars        |
/// | Terminal (macOS) | 2 chars        |
/// | VS Code terminal | 4 chars        |
/// | GNOME Terminal   | 4 chars        |
///
/// If we really wanted to, we could try and detect the terminal being used and
/// adjust the width accordingly. However, this is probably not worth the
/// effort.
fn display_width(str: &str) -> usize {
  let num_tabs = str.chars().filter(|c| *c == '\t').count();
  str.width_cjk() + num_tabs * TAB_WIDTH - num_tabs
}

pub struct DiagnosticDisplay<'a, T: Diagnostic + ?Sized> {
  diagnostic: &'a T,
}

impl<T: Diagnostic + ?Sized> Display for DiagnosticDisplay<'_, T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    print_diagnostic(f, self.diagnostic)
  }
}

// error[missing-return-type]: missing explicit return type on public function
//   at /mnt/artemis/Projects/github.com/denoland/deno/test.ts:1:16
//    |
//  1 | export function test() {
//    |                 ^^^^
//    = hint: add an explicit return type to the function
//    |
//  1 | export function test(): string {
//    |                       ^^^^^^^^
//
//   info: all functions that are exported from a module must have an explicit return type to support fast check and documentation generation.
//   docs: https://jsr.io/d/missing-return-type
fn print_diagnostic(
  io: &mut dyn std::fmt::Write,
  diagnostic: &(impl Diagnostic + ?Sized),
) -> Result<(), std::fmt::Error> {
  match diagnostic.level() {
    DiagnosticLevel::Error => {
      write!(
        io,
        "{}",
        colors::red_bold(format_args!("error[{}]", diagnostic.code()))
      )?;
    }
    DiagnosticLevel::Warning => {
      write!(
        io,
        "{}",
        colors::yellow_bold(format_args!("warning[{}]", diagnostic.code()))
      )?;
    }
  }

  writeln!(io, ": {}", colors::bold(diagnostic.message()))?;

  let mut max_line_number_digits = 1;
  if let Some(snippet) = diagnostic.snippet() {
    for highlight in snippet.highlights.iter() {
      let last_line = line_number(&snippet.source, highlight.range.end);
      max_line_number_digits =
        max_line_number_digits.max(last_line.ilog10() + 1);
    }
  }

  if let Some(snippet) = diagnostic.snippet_fixed() {
    for highlight in snippet.highlights.iter() {
      let last_line = line_number(&snippet.source, highlight.range.end);
      max_line_number_digits =
        max_line_number_digits.max(last_line.ilog10() + 1);
    }
  }

  let location = diagnostic.location();
  write!(
    io,
    "{}{}",
    RepeatingCharFmt(' ', max_line_number_digits as usize),
    colors::intense_blue("-->"),
  )?;
  match &location {
    DiagnosticLocation::Path { path } => {
      write!(io, " {}", colors::cyan(path.display()))?;
    }
    DiagnosticLocation::Module { specifier }
    | DiagnosticLocation::ModulePosition { specifier, .. } => {
      if let Some(path) = specifier_to_file_path(specifier) {
        write!(io, " {}", colors::cyan(path.display()))?;
      } else {
        write!(io, " {}", colors::cyan(specifier.as_str()))?;
      }
    }
  }
  if let Some((line, column)) = location.position() {
    write!(
      io,
      "{}",
      colors::yellow(format_args!(":{}:{}", line, column))
    )?;
  }

  if diagnostic.snippet().is_some()
    || diagnostic.hint().is_some()
    || diagnostic.snippet_fixed().is_some()
    || !diagnostic.info().is_empty()
    || diagnostic.docs_url().is_some()
  {
    writeln!(io)?;
  }

  if let Some(snippet) = diagnostic.snippet() {
    print_snippet(io, &snippet, max_line_number_digits)?;
  };

  if let Some(hint) = diagnostic.hint() {
    write!(
      io,
      "{} {} ",
      RepeatingCharFmt(' ', max_line_number_digits as usize),
      colors::intense_blue("=")
    )?;
    writeln!(io, "{}: {}", colors::bold("hint"), hint)?;
  }

  if let Some(snippet) = diagnostic.snippet_fixed() {
    print_snippet(io, &snippet, max_line_number_digits)?;
  }

  if !diagnostic.info().is_empty() || diagnostic.docs_url().is_some() {
    writeln!(io)?;
  }

  for info in diagnostic.info().iter() {
    writeln!(io, "  {}: {}", colors::intense_blue("info"), info)?;
  }
  if let Some(docs_url) = diagnostic.docs_url() {
    writeln!(io, "  {}: {}", colors::intense_blue("docs"), docs_url)?;
  }

  Ok(())
}

/// Prints a snippet to the given writer and returns the line number indent.
fn print_snippet(
  io: &mut dyn std::fmt::Write,
  snippet: &DiagnosticSnippet<'_>,
  max_line_number_digits: u32,
) -> Result<(), std::fmt::Error> {
  let DiagnosticSnippet { source, highlights } = snippet;

  fn print_padded(
    io: &mut dyn std::fmt::Write,
    text: impl std::fmt::Display,
    padding: u32,
  ) -> Result<(), std::fmt::Error> {
    for _ in 0..padding {
      write!(io, " ")?;
    }
    write!(io, "{}", text)?;
    Ok(())
  }

  let mut lines_to_show = HashMap::<usize, Vec<usize>>::new();
  let mut highlights_info = Vec::new();
  for (i, highlight) in highlights.iter().enumerate() {
    let start_line_number = line_number(source, highlight.range.start);
    let end_line_number = line_number(source, highlight.range.end);
    highlights_info.push((start_line_number, end_line_number));
    for line_number in start_line_number..=end_line_number {
      lines_to_show.entry(line_number).or_default().push(i);
    }
  }

  let mut lines_to_show = lines_to_show.into_iter().collect::<Vec<_>>();
  lines_to_show.sort();

  print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
  writeln!(io)?;
  let mut previous_line_number = None;
  let mut previous_line_empty = false;
  for (line_number, highlight_indexes) in lines_to_show {
    if previous_line_number.is_some()
      && previous_line_number == Some(line_number - 1)
      && !previous_line_empty
    {
      print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
      writeln!(io)?;
    }

    print_padded(
      io,
      colors::intense_blue(format_args!("{} | ", line_number)),
      max_line_number_digits - line_number.ilog10() - 1,
    )?;

    let line_start_pos = source.line_start(line_number - 1);
    let line_end_pos = source.line_end(line_number - 1);
    let line_text = line_text(source, line_number);
    writeln!(io, "{}", ReplaceTab(line_text))?;
    previous_line_empty = false;

    let mut wrote_description = false;
    for highlight_index in highlight_indexes {
      let highlight = &highlights[highlight_index];
      let (start_line_number, end_line_number) =
        highlights_info[highlight_index];

      let padding_width;
      let highlight_width;
      if start_line_number == end_line_number {
        padding_width = display_width(source.range_text(&SourceRange::new(
          line_start_pos,
          highlight.range.start.pos(source),
        )));
        highlight_width = display_width(source.range_text(&SourceRange::new(
          highlight.range.start.pos(source),
          highlight.range.end.pos(source),
        )));
      } else if start_line_number == line_number {
        padding_width = display_width(source.range_text(&SourceRange::new(
          line_start_pos,
          highlight.range.start.pos(source),
        )));
        highlight_width = display_width(source.range_text(&SourceRange::new(
          highlight.range.start.pos(source),
          line_end_pos,
        )));
      } else if end_line_number == line_number {
        padding_width = 0;
        highlight_width = display_width(source.range_text(&SourceRange::new(
          line_start_pos,
          highlight.range.end.pos(source),
        )));
      } else {
        padding_width = 0;
        highlight_width = display_width(line_text);
      }

      let underline =
        RepeatingCharFmt(highlight.style.underline_char(), highlight_width);
      print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
      write!(io, "{}", RepeatingCharFmt(' ', padding_width))?;
      write!(io, "{}", highlight.style.style_underline(underline))?;

      if line_number == end_line_number {
        if let Some(description) = &highlight.description {
          write!(io, " {}", highlight.style.style_underline(description))?;
          wrote_description = true;
        }
      }

      writeln!(io)?;
    }

    if wrote_description {
      print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
      writeln!(io)?;
      previous_line_empty = true;
    }

    previous_line_number = Some(line_number);
  }

  Ok(())
}

/// Attempts to convert a specifier to a file path. By default, uses the Url
/// crate's `to_file_path()` method, but falls back to try and resolve unix-style
/// paths on Windows.
fn specifier_to_file_path(specifier: &ModuleSpecifier) -> Option<PathBuf> {
  fn to_file_path_if_not_wasm(_specifier: &ModuleSpecifier) -> Option<PathBuf> {
    #[cfg(target_arch = "wasm32")]
    {
      None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
      // not available in Wasm
      _specifier.to_file_path().ok()
    }
  }

  if specifier.scheme() != "file" {
    None
  } else if cfg!(windows) {
    match to_file_path_if_not_wasm(specifier) {
      Some(path) => Some(path),
      None => {
        // This might be a unix-style path which is used in the tests even on Windows.
        // Attempt to see if we can convert it to a `PathBuf`. This code should be removed
        // once/if https://github.com/servo/rust-url/issues/730 is implemented.
        if specifier.scheme() == "file"
          && specifier.host().is_none()
          && specifier.port().is_none()
          && specifier.path_segments().is_some()
        {
          let path_str = specifier.path();
          match String::from_utf8(
            percent_encoding::percent_decode(path_str.as_bytes()).collect(),
          ) {
            Ok(path_str) => Some(PathBuf::from(path_str)),
            Err(_) => None,
          }
        } else {
          None
        }
      }
    }
  } else {
    to_file_path_if_not_wasm(specifier)
  }
}

#[cfg(test)]
mod tests {
  use std::borrow::Cow;

  use super::*;
  use crate::ModuleSpecifier;
  use crate::SourceTextInfo;

  #[test]
  fn test_display_width() {
    assert_eq!(display_width("abc"), 3);
    assert_eq!(display_width("\t"), 2);
    assert_eq!(display_width("\t\t123"), 7);
    assert_eq!(display_width("🎄"), 2);
    assert_eq!(display_width("🎄🎄"), 4);
    assert_eq!(display_width("🧑‍🦰"), 4);
  }

  #[test]
  fn test_position_in_file_from_text_info_simple() {
    let specifier: ModuleSpecifier = "file:///dev/test.ts".parse().unwrap();
    let text_info = SourceTextInfo::new("foo\nbar\nbaz".into());
    let pos = text_info.line_start(1);
    let location = DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(&specifier),
      source_pos: DiagnosticSourcePos::SourcePos(pos),
      text_info: Cow::Owned(text_info),
    };
    let position = location.position().unwrap();
    assert_eq!(position, (2, 1))
  }

  #[test]
  fn test_position_in_file_from_text_info_emoji() {
    let specifier: ModuleSpecifier = "file:///dev/test.ts".parse().unwrap();
    let text_info = SourceTextInfo::new("🧑‍🦰text".into());
    let pos = text_info.line_start(0) + 11; // the end of the emoji
    let location = DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(&specifier),
      source_pos: DiagnosticSourcePos::SourcePos(pos),
      text_info: Cow::Owned(text_info),
    };
    let position = location.position().unwrap();
    assert_eq!(position, (1, 6))
  }

  #[test]
  fn test_specifier_to_file_path() {
    run_success_test("file:///", "/");
    run_success_test("file:///test", "/test");
    run_success_test("file:///dir/test/test.txt", "/dir/test/test.txt");
    run_success_test(
      "file:///dir/test%20test/test.txt",
      "/dir/test test/test.txt",
    );

    fn run_success_test(specifier: &str, expected_path: &str) {
      let result =
        specifier_to_file_path(&ModuleSpecifier::parse(specifier).unwrap())
          .unwrap();
      assert_eq!(result, PathBuf::from(expected_path));
    }
  }
}
