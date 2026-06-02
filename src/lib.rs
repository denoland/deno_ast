// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]
#![deny(clippy::unnecessary_wraps)]
#![deny(clippy::print_stderr)]
#![deny(clippy::print_stdout)]

/// Version of this crate, which may be useful for emit caches.
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

#[cfg(feature = "cjs")]
mod cjs_parse;
mod comments;
#[cfg(feature = "dep_analysis")]
pub mod dep;
pub mod diagnostics;
#[cfg(feature = "emit")]
mod emit;
mod exports;
mod lexing;
mod parsed_source;
mod parsing;
#[cfg(feature = "scopes")]
mod scopes;
mod source_range;
mod text_changes;
#[cfg(feature = "transpiling")]
mod transpiling;
mod types;

#[cfg(feature = "cjs")]
pub use cjs_parse::*;
pub use comments::*;
pub use deno_media_type::*;
#[cfg(feature = "emit")]
pub use emit::*;
pub use exports::*;
pub use lexing::*;
pub use parsed_source::*;
pub use parsing::*;
#[cfg(feature = "scopes")]
pub use scopes::*;
pub use source_range::*;
pub use text_changes::*;
#[cfg(feature = "transpiling")]
pub use transpiling::*;
pub use types::*;

pub type ModuleSpecifier = url::Url;

/// Re-exports of oxc crates for consumers that need direct access.
pub mod oxc {
  pub use oxc::allocator;
  pub use oxc::ast;
  #[cfg(feature = "visit")]
  pub use oxc::ast_visit;
  #[cfg(feature = "codegen")]
  pub use oxc::codegen;
  pub use oxc::diagnostics;
  pub use oxc::parser;
  #[cfg(feature = "semantic")]
  pub use oxc::semantic;
  /// Re-export of `oxc::span` plus the string types (`Str`, `Ident`,
  /// `CompactStr`) that moved to `oxc_str` in oxc 0.134.
  pub mod span {
    pub use oxc::span::*;
    pub use oxc_str::{CompactStr, Ident, Str};
  }
  pub use oxc::syntax;
  #[cfg(feature = "transpiling")]
  pub use oxc::transformer;
}
