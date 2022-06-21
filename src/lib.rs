// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

#![deny(clippy::disallowed_methods)]
#![deny(clippy::disallowed_types)]

#[cfg(feature = "cjs")]
mod cjs_parse;
mod comments;
mod lexing;
mod media_type;
mod parsed_source;
mod parsing;
#[cfg(feature = "scopes")]
mod scopes;
mod text_changes;
#[cfg(feature = "transpiling")]
mod transpiling;
mod types;

#[cfg(feature = "view")]
pub use dprint_swc_ext::view;

pub use dprint_swc_ext::common::*;

#[cfg(feature = "cjs")]
pub use cjs_parse::*;
pub use comments::*;
pub use lexing::*;
pub use media_type::*;
pub use parsed_source::*;
pub use parsing::*;
#[cfg(feature = "scopes")]
pub use scopes::*;
pub use text_changes::*;
#[cfg(feature = "transpiling")]
pub use transpiling::*;
pub use types::*;

pub mod swc {
  pub use dprint_swc_ext::swc::atoms;
  pub use dprint_swc_ext::swc::common;
  #[cfg(feature = "bundler")]
  pub use swc_bundler as bundler;
  pub use swc_ecma_ast as ast;
  #[cfg(feature = "codegen")]
  pub use swc_ecma_codegen as codegen;
  #[cfg(feature = "dep_graph")]
  pub use swc_ecma_dep_graph as dep_graph;
  #[cfg(feature = "minifier")]
  pub use swc_ecma_minifier as minifier;
  pub use swc_ecma_parser as parser;
  #[cfg(feature = "preset_env")]
  pub use swc_ecma_preset_env as preset_env;
  #[cfg(feature = "transforms")]
  pub mod transforms {
    pub use self::fixer::fixer;
    pub use self::hygiene::hygiene;
    pub use swc_ecma_transforms_base::assumptions::Assumptions;
    pub use swc_ecma_transforms_base::fixer;
    pub use swc_ecma_transforms_base::helpers;
    pub use swc_ecma_transforms_base::hygiene;
    pub use swc_ecma_transforms_base::pass;
    pub use swc_ecma_transforms_base::perf;
    pub use swc_ecma_transforms_base::resolver;

    #[cfg(feature = "compat")]
    pub use swc_ecma_transforms_compat as compat;
    #[cfg(feature = "proposal")]
    pub use swc_ecma_transforms_proposal as proposal;
    #[cfg(feature = "react")]
    pub use swc_ecma_transforms_react as react;
    #[cfg(feature = "typescript")]
    pub use swc_ecma_transforms_typescript as typescript;
  }
  #[cfg(feature = "utils")]
  pub use swc_ecma_utils as utils;
  #[cfg(feature = "visit")]
  pub use swc_ecma_visit as visit;
}
