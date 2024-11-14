// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use base64::Engine;
use thiserror::Error;

use crate::swc::codegen::text_writer::JsWriter;
use crate::swc::codegen::Node;
use crate::swc::common::FileName;
use crate::ModuleSpecifier;
use crate::ProgramRef;
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

#[derive(Debug, Clone, Hash)]
pub struct EmitOptions {
  /// How and if source maps should be generated.
  pub source_map: SourceMapOption,
  /// Base url to use for source maps.
  ///
  /// When a base is provided, when mapping source names in the source map, the
  /// name will be relative to the base.
  pub source_map_base: Option<ModuleSpecifier>,
  /// The `"file"` field of the generated source map.
  pub source_map_file: Option<String>,
  /// Whether to inline the source contents in the source map. Defaults to `true`.
  pub inline_sources: bool,
  /// Whether to remove comments in the output. Defaults to `false`.
  pub remove_comments: bool,
}

impl Default for EmitOptions {
  fn default() -> Self {
    EmitOptions {
      source_map: SourceMapOption::default(),
      source_map_base: None,
      source_map_file: None,
      inline_sources: true,
      remove_comments: false,
    }
  }
}

/// Source emitted based on the emit options.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Debug)]
pub struct EmittedSourceText {
  /// Emitted text as utf8 bytes.
  pub text: String,
  /// Source map back to the original file.
  pub source_map: Option<String>,
}

#[derive(Debug, Error, deno_error::JsError)]
pub enum EmitError {
  #[class(inherit)]
  #[error(transparent)]
  SwcEmit(#[inherit] std::io::Error),
  #[error(transparent)]
  SourceMap(sourcemap::Error),
  #[class(TYPE)]
  #[error(transparent)]
  SourceMapEncode(base64::EncodeSliceError),
}

/// Emits the program as a string of JavaScript code, possibly with the passed
/// comments, and optionally also a source map.
pub fn emit(
  program: ProgramRef,
  comments: &dyn crate::swc::common::comments::Comments,
  source_map: &SourceMap,
  emit_options: &EmitOptions,
) -> Result<EmittedSourceText, EmitError> {
  let source_map = source_map.inner();
  let mut src_map_buf = vec![];
  let mut src_buf = vec![];
  {
    let mut writer = Box::new(JsWriter::new(
      source_map.clone(),
      "\n",
      &mut src_buf,
      Some(&mut src_map_buf),
    ));
    writer.set_indent_str("  "); // two spaces

    let mut emitter = crate::swc::codegen::Emitter {
      cfg: swc_codegen_config(),
      comments: if emit_options.remove_comments {
        None
      } else {
        Some(&comments)
      },
      cm: source_map.clone(),
      wr: writer,
    };
    match program {
      ProgramRef::Module(n) => {
        n.emit_with(&mut emitter).map_err(EmitError::SwcEmit)?;
      }
      ProgramRef::Script(n) => {
        n.emit_with(&mut emitter).map_err(EmitError::SwcEmit)?;
      }
    }
  }

  let mut map: Option<Vec<u8>> = None;

  if emit_options.source_map != SourceMapOption::None {
    let mut map_buf = Vec::new();
    let source_map_config = SourceMapConfig {
      inline_sources: emit_options.inline_sources,
      maybe_base: emit_options.source_map_base.as_ref(),
    };
    let mut source_map = source_map.build_source_map_with_config(
      &src_map_buf,
      None,
      source_map_config,
    );
    if let Some(file) = &emit_options.source_map_file {
      source_map.set_file(Some(file.to_string()));
    }
    source_map
      .to_writer(&mut map_buf)
      .map_err(EmitError::SourceMap)?;

    if emit_options.source_map == SourceMapOption::Inline {
      // length is from the base64 crate examples
      let mut inline_buf = vec![0; map_buf.len() * 4 / 3 + 4];
      let size = base64::prelude::BASE64_STANDARD
        .encode_slice(map_buf, &mut inline_buf)
        .map_err(EmitError::SourceMapEncode)?;
      let inline_buf = &inline_buf[..size];
      let prelude_text = "//# sourceMappingURL=data:application/json;base64,";
      let src_has_trailing_newline = src_buf.ends_with(b"\n");
      let additional_capacity = if src_has_trailing_newline { 0 } else { 1 }
        + prelude_text.len()
        + inline_buf.len();
      let expected_final_capacity = src_buf.len() + additional_capacity;
      src_buf.reserve(additional_capacity);
      if !src_has_trailing_newline {
        src_buf.push(b'\n');
      }
      src_buf.extend(prelude_text.as_bytes());
      src_buf.extend(inline_buf);
      debug_assert_eq!(src_buf.len(), expected_final_capacity);
    } else {
      map = Some(map_buf);
    }
  }

  debug_assert!(std::str::from_utf8(&src_buf).is_ok(), "valid utf-8");
  if let Some(map) = &map {
    debug_assert!(std::str::from_utf8(map).is_ok(), "valid utf-8");
  }

  // It's better to return a string here because then we can pass this to deno_core/v8
  // as a known string, so it doesn't need to spend any time analyzing it.
  Ok(EmittedSourceText {
    // SAFETY: swc appends UTF-8 bytes to the JsWriter, so we can safely assume
    // that the final string is UTF-8 (unchecked for performance reasons)
    text: unsafe { String::from_utf8_unchecked(src_buf) },
    // SAFETY: see above comment
    source_map: map.map(|b| unsafe { String::from_utf8_unchecked(b) }),
  })
}

/// Implements a configuration trait for source maps that reflects the logic
/// to embed sources in the source map or not.
#[derive(Debug)]
pub struct SourceMapConfig<'a> {
  pub inline_sources: bool,
  pub maybe_base: Option<&'a ModuleSpecifier>,
}

impl<'a> crate::swc::common::source_map::SourceMapGenConfig
  for SourceMapConfig<'a>
{
  fn file_name_to_source(&self, f: &FileName) -> String {
    match f {
      FileName::Url(specifier) => self
        .maybe_base
        .and_then(|base| {
          debug_assert!(
            base.as_str().ends_with('/'),
            "source map base should end with a slash"
          );
          base.make_relative(specifier)
        })
        .filter(|relative| !relative.is_empty())
        .unwrap_or_else(|| f.to_string()),
      _ => f.to_string(),
    }
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
  config.target = crate::ES_VERSION;
  config.ascii_only = false;
  config.minify = false;
  config.omit_last_semi = false;
  config.emit_assert_for_import_attributes = false;
  config.inline_script = false;
  config
}
