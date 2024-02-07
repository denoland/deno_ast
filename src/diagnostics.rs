// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use std::borrow::Cow;
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

impl<'a> DiagnosticLocation<'a> {
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
  /// The piece of the snippet that should be highlighted.
  pub highlight: DiagnosticSnippetHighlight<'a>,
}

pub struct DiagnosticSnippetHighlight<'a> {
  /// The range of the snippet that should be highlighted.
  pub range: DiagnosticSourceRange,
  /// The style of the highlight.
  pub style: DiagnosticSnippetHighlightStyle,
  /// An optional inline description of the highlight.
  pub description: Option<Cow<'a, str>>,
}

pub enum DiagnosticSnippetHighlightStyle {
  /// The highlight is an error. This will place red carets under the highlight.
  Error,
  #[allow(dead_code)]
  /// The highlight is a warning. This will place yellow carets under the
  /// highlight.
  Warning,
  #[allow(dead_code)]
  /// The highlight shows code additions. This will place green + signs under
  /// the highlight and will highlight the code in green.
  Addition,
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
      DiagnosticSnippetHighlightStyle::Addition => colors::green_bold(s),
      DiagnosticSnippetHighlightStyle::Hint => colors::intense_blue(s),
    }
  }

  fn underline_char(&self) -> char {
    match self {
      DiagnosticSnippetHighlightStyle::Error => '^',
      DiagnosticSnippetHighlightStyle::Warning => '^',
      DiagnosticSnippetHighlightStyle::Addition => '+',
      DiagnosticSnippetHighlightStyle::Hint => '-',
    }
  }
}

/// Returns the text of the line with the given number.
fn line_text(source: &SourceTextInfo, line_number: usize) -> &str {
  source.line_text(line_number - 1)
}

/// Returns the text of the line that contains the given position, split at the
/// given position.
fn line_text_split(
  source: &SourceTextInfo,
  pos: DiagnosticSourcePos,
) -> (&str, &str) {
  let pos = pos.pos(source);
  let line_index = source.line_index(pos);
  let line_start_pos = source.line_start(line_index);
  let line_end_pos = source.line_end(line_index);
  let before = source.range_text(&SourceRange::new(line_start_pos, pos));
  let after = source.range_text(&SourceRange::new(pos, line_end_pos));
  (before, after)
}

/// Returns the text of the line that contains the given positions, split at the
/// given positions.
///
/// If the positions are on different lines, this will panic.
fn line_text_split3(
  source: &SourceTextInfo,
  start_pos: DiagnosticSourcePos,
  end_pos: DiagnosticSourcePos,
) -> (&str, &str, &str) {
  let start_pos = start_pos.pos(source);
  let end_pos = end_pos.pos(source);
  let line_index = source.line_index(start_pos);
  assert_eq!(
    line_index,
    source.line_index(end_pos),
    "start and end must be on the same line"
  );
  let line_start_pos = source.line_start(line_index);
  let line_end_pos = source.line_end(line_index);
  let before = source.range_text(&SourceRange::new(line_start_pos, start_pos));
  let between = source.range_text(&SourceRange::new(start_pos, end_pos));
  let after = source.range_text(&SourceRange::new(end_pos, line_end_pos));
  (before, between, after)
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
  fn code(&self) -> impl fmt::Display + '_;

  /// The human-readable diagnostic message.
  fn message(&self) -> impl fmt::Display + '_;

  /// The location this diagnostic is associated with.
  fn location(&self) -> DiagnosticLocation;

  /// A snippet showing the source code associated with the diagnostic.
  fn snippet(&self) -> Option<DiagnosticSnippet<'_>>;

  /// A hint for fixing the diagnostic.
  fn hint(&self) -> Option<impl fmt::Display + '_>;

  /// A snippet showing how the diagnostic can be fixed.
  fn snippet_fixed(&self) -> Option<DiagnosticSnippet<'_>>;

  fn info(&self) -> Cow<'_, [Cow<'_, str>]>;

  /// An optional URL to the documentation for the diagnostic.
  fn docs_url(&self) -> Option<impl fmt::Display + '_>;

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
  str.width_cjk() + (str.chars().filter(|c| *c == '\t').count() * TAB_WIDTH)
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
    let last_line = line_number(&snippet.source, snippet.highlight.range.end);
    max_line_number_digits = max_line_number_digits.max(last_line.ilog10() + 1);
  }
  if let Some(snippet) = diagnostic.snippet_fixed() {
    let last_line = line_number(&snippet.source, snippet.highlight.range.end);
    max_line_number_digits = max_line_number_digits.max(last_line.ilog10() + 1);
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
      if let Ok(path) = specifier.to_file_path() {
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
  writeln!(io)?;

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

  writeln!(io)?;

  let mut needs_final_newline = false;
  for info in diagnostic.info().iter() {
    needs_final_newline = true;
    writeln!(io, "  {}: {}", colors::intense_blue("info"), info)?;
  }
  if let Some(docs_url) = diagnostic.docs_url() {
    needs_final_newline = true;
    writeln!(io, "  {}: {}", colors::intense_blue("docs"), docs_url)?;
  }

  if needs_final_newline {
    writeln!(io)?;
  }

  Ok(())
}

/// Prints a snippet to the given writer and returns the line number indent.
fn print_snippet(
  io: &mut dyn std::fmt::Write,
  snippet: &DiagnosticSnippet<'_>,
  max_line_number_digits: u32,
) -> Result<(), std::fmt::Error> {
  let DiagnosticSnippet { source, highlight } = snippet;

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

  let start_line_number = line_number(&snippet.source, highlight.range.start);
  let end_line_number = line_number(&snippet.source, highlight.range.end);

  print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
  writeln!(io)?;
  for line_number in start_line_number..=end_line_number {
    print_padded(
      io,
      colors::intense_blue(format_args!("{} | ", line_number)),
      max_line_number_digits - line_number.ilog10() - 1,
    )?;

    let padding_width;
    let highlight_width;
    if line_number == start_line_number && start_line_number == end_line_number
    {
      let (before, between, after) =
        line_text_split3(source, highlight.range.start, highlight.range.end);
      write!(io, "{}", ReplaceTab(before))?;
      match highlight.style {
        DiagnosticSnippetHighlightStyle::Addition => {
          write!(io, "{}", colors::green(ReplaceTab(between)))?;
        }
        _ => {
          write!(io, "{}", ReplaceTab(between))?;
        }
      }
      writeln!(io, "{}", ReplaceTab(after))?;
      padding_width = display_width(before);
      highlight_width = display_width(between);
    } else if line_number == start_line_number {
      let (before, after) = line_text_split(source, highlight.range.start);
      write!(io, "{}", ReplaceTab(before))?;
      match highlight.style {
        DiagnosticSnippetHighlightStyle::Addition => {
          write!(io, "{}", colors::green(ReplaceTab(after)))?;
        }
        _ => {
          write!(io, "{}", ReplaceTab(after))?;
        }
      }
      writeln!(io)?;
      padding_width = display_width(before);
      highlight_width = display_width(after);
    } else if line_number == end_line_number {
      let (before, after) = line_text_split(source, highlight.range.end);
      match highlight.style {
        DiagnosticSnippetHighlightStyle::Addition => {
          write!(io, "{}", colors::green(ReplaceTab(before)))?;
        }
        _ => {
          write!(io, "{}", ReplaceTab(before))?;
        }
      }
      write!(io, "{}", ReplaceTab(after))?;
      writeln!(io)?;
      padding_width = 0;
      highlight_width = display_width(before);
    } else {
      let line = line_text(source, line_number);
      writeln!(io, "{}", ReplaceTab(line))?;
      padding_width = 0;
      highlight_width = display_width(line);
    }

    print_padded(io, colors::intense_blue(" | "), max_line_number_digits)?;
    write!(io, "{}", RepeatingCharFmt(' ', padding_width))?;
    let underline =
      RepeatingCharFmt(highlight.style.underline_char(), highlight_width);
    write!(io, "{}", highlight.style.style_underline(underline))?;

    if line_number == end_line_number {
      if let Some(description) = &highlight.description {
        write!(io, " {}", highlight.style.style_underline(description))?;
      }
    }

    writeln!(io)?;
  }

  Ok(())
}

#[cfg(test)]
mod tests {
  use std::borrow::Cow;

  use crate::ModuleSpecifier;
  use crate::SourceTextInfo;

  #[test]
  fn test_display_width() {
    assert_eq!(super::display_width("abc"), 3);
    assert_eq!(super::display_width("\t"), 2);
    assert_eq!(super::display_width("\t\t123"), 7);
    assert_eq!(super::display_width("🎄"), 2);
    assert_eq!(super::display_width("🎄🎄"), 4);
    assert_eq!(super::display_width("🧑‍🦰"), 4);
  }

  #[test]
  fn test_position_in_file_from_text_info_simple() {
    let specifier: ModuleSpecifier = "file:///dev/test.ts".parse().unwrap();
    let text_info = SourceTextInfo::new("foo\nbar\nbaz".into());
    let pos = text_info.line_start(1);
    let location = super::DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(&specifier),
      source_pos: super::DiagnosticSourcePos::SourcePos(pos),
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
    let location = super::DiagnosticLocation::ModulePosition {
      specifier: Cow::Borrowed(&specifier),
      source_pos: super::DiagnosticSourcePos::SourcePos(pos),
      text_info: Cow::Owned(text_info),
    };
    let position = location.position().unwrap();
    assert_eq!(position, (1, 6))
  }
}
