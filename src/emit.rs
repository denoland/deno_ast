// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use anyhow::Result;
use base64::Engine;

use crate::swc::ast::Program;
use crate::swc::codegen::text_writer::JsWriter;
use crate::swc::codegen::Node;
use crate::swc::common::FileName;
use crate::SourceMap;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SourceMapOption {
  /// Source map should be inlined into the source (default)
  #[default]
  Inline,
  /// Source map should be generated as a separate file.
  Separate,
  /// Source map should not be generated at all.
  None,
}

#[derive(Debug, Clone, Copy, Hash)]
pub struct EmitOptions {
  /// How and if source maps should be generated.
  pub source_map: SourceMapOption,
  /// Whether to inline the source contents in the source map. Defaults to `true`.
  pub inline_sources: bool,
  /// Whether to keep comments in the output. Defaults to `false`.
  pub keep_comments: bool,
}

impl Default for EmitOptions {
  fn default() -> Self {
    EmitOptions {
      source_map: SourceMapOption::default(),
      inline_sources: true,
      keep_comments: false,
    }
  }
}

/// Source emitted based on the emit options.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct EmittedSource {
  /// Emitted text.
  pub text: String,
  /// Source map back to the original file.
  pub source_map: Option<String>,
}

/// Emits the program as a string of JavaScript code, possibly with the passed
/// comments, and optionally also a source map.
pub fn emit(
  program: &Program,
  comments: &dyn crate::swc::common::comments::Comments,
  source_map: &SourceMap,
  emit_options: &EmitOptions,
) -> Result<EmittedSource> {
  let source_map = source_map.inner();
  let mut src_map_buf = vec![];
  let mut buf = vec![];
  {
    let mut writer = Box::new(JsWriter::new(
      source_map.clone(),
      "\n",
      &mut buf,
      Some(&mut src_map_buf),
    ));
    writer.set_indent_str("  "); // two spaces

    let mut emitter = crate::swc::codegen::Emitter {
      cfg: swc_codegen_config(),
      comments: if emit_options.keep_comments {
        Some(&comments)
      } else {
        None
      },
      cm: source_map.clone(),
      wr: writer,
    };
    program.emit_with(&mut emitter)?;
  }

  let mut src = String::from_utf8(buf)?;
  let mut map: Option<String> = None;

  if emit_options.source_map != SourceMapOption::None {
    let mut buf = Vec::new();
    let source_map_config = SourceMapConfig {
      inline_sources: emit_options.inline_sources,
    };
    source_map
      .build_source_map_with_config(&src_map_buf, None, source_map_config)
      .to_writer(&mut buf)?;

    if emit_options.source_map == SourceMapOption::Inline {
      if !src.ends_with('\n') {
        src.push('\n');
      }
      src.push_str("//# sourceMappingURL=data:application/json;base64,");
      base64::prelude::BASE64_STANDARD.encode_string(buf, &mut src);
    } else {
      map = Some(String::from_utf8(buf)?);
    }
  }

  Ok(EmittedSource {
    text: src,
    source_map: map,
  })
}

/// Implements a configuration trait for source maps that reflects the logic
/// to embed sources in the source map or not.
#[derive(Debug)]
pub struct SourceMapConfig {
  pub inline_sources: bool,
}

impl crate::swc::common::source_map::SourceMapGenConfig for SourceMapConfig {
  fn file_name_to_source(&self, f: &FileName) -> String {
    f.to_string()
  }

  fn inline_sources_content(&self, f: &FileName) -> bool {
    match f {
      FileName::Real(..) | FileName::Custom(..) => false,
      FileName::Url(..) => self.inline_sources,
      _ => true,
    }
  }
}

pub fn swc_codegen_config() -> crate::swc::codegen::Config {
  // NOTICE ON UPGRADE: This struct has #[non_exhaustive] on it,
  // which prevents creating a struct expr here. For that reason,
  // inspect the struct on swc upgrade and explicitly specify any
  // new options here in order to ensure we maintain these settings.
  let mut config = crate::swc::codegen::Config::default();
  config.minify = false;
  config.ascii_only = false;
  config.omit_last_semi = false;
  config.target = crate::ES_VERSION;
  config.emit_assert_for_import_attributes = false;
  config
}
