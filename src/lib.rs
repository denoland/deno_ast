// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

mod comments;
mod lexing;
mod media_type;
mod parsed_source;
mod parsing;
mod text_encoding;
mod text_info;
mod types;

#[cfg(feature = "view")]
pub use dprint_swc_ecma_ast_view as view;

pub use comments::*;
pub use lexing::*;
pub use media_type::*;
pub use parsed_source::*;
pub use parsing::*;
pub use text_info::*;
pub use types::*;

pub mod swc {
  pub use swc_atoms as atoms;
  #[cfg(feature = "bundler")]
  pub use swc_bundler as bundler;
  pub use swc_common as common;
  pub use swc_ecmascript::ast;
  #[cfg(feature = "codegen")]
  pub use swc_ecmascript::codegen;
  #[cfg(feature = "dep_graph")]
  pub use swc_ecmascript::dep_graph;
  #[cfg(feature = "minifier")]
  pub use swc_ecmascript::minifier;
  pub use swc_ecmascript::parser;
  #[cfg(feature = "preset_env")]
  pub use swc_ecmascript::preset_env;
  #[cfg(feature = "transforms")]
  pub use swc_ecmascript::transforms;
  #[cfg(feature = "utils")]
  pub use swc_ecmascript::utils;
  #[cfg(feature = "visit")]
  pub use swc_ecmascript::visit;
}
