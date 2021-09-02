// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use std::fmt;

pub type LineAndColumnDisplay = text_lines::LineAndColumnDisplay;
pub type LineAndColumnIndex = text_lines::LineAndColumnIndex;

#[derive(Debug, Clone)]
pub struct Diagnostic {
  pub specifier: String,
  pub display_position: LineAndColumnDisplay,
  pub message: String,
}

impl std::error::Error for Diagnostic {}

impl fmt::Display for Diagnostic {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(
      f,
      "{} at {}:{}:{}",
      self.message, self.specifier, self.display_position.line_number, self.display_position.column_number
    )
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum MediaType {
  JavaScript = 0,
  Jsx = 1,
  TypeScript = 2,
  Dts = 3,
  Tsx = 4,
  Json = 5,
  Wasm = 6,
  TsBuildInfo = 7,
  SourceMap = 8,
  Unknown = 9,
}
