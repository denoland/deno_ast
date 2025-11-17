// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use deno_error::JsError;
use swc_ecma_lexer::TsSyntax;
use thiserror::Error;
use url::Url;

use crate::diagnostics::DiagnosticCollector;
use crate::diagnostics::SwcFoldDiagnosticsError;
use crate::diagnostics::ensure_no_fatal_swc_diagnostics;
use crate::swc::common::FileName;
use crate::swc::common::SourceMap;
use crate::swc::common::sync::Lrc;

pub struct TypeStripOptions {
  pub module: Option<bool>,
}

#[derive(Debug, Error, JsError)]
pub enum TypeStripError {
  #[class(type)]
  #[error(transparent)]
  Fold(#[from] SwcFoldDiagnosticsError),
  #[class(type)]
  #[error(transparent)]
  TsError(#[from] swc_ts_fast_strip::TsError),
}

/// Type stripts the given source.
pub fn type_strip(
  filename: &Url,
  code: String,
  options: TypeStripOptions,
) -> Result<String, TypeStripError> {
  let source_map = Lrc::new(SourceMap::default());
  source_map.new_source_file(
    Lrc::new(FileName::Url(filename.clone())),
    code.to_string(),
  );

  let emitter = DiagnosticCollector::default();
  let (handler, diagnostics_cell) = emitter.into_handler_and_cell();
  let output = crate::swc::common::errors::HANDLER.set(&handler, || {
    swc_ts_fast_strip::operate(
      &source_map,
      &handler,
      code,
      swc_ts_fast_strip::Options {
        module: options.module,
        filename: Some(filename.to_string()),
        parser: TsSyntax {
          decorators: false,
          disallow_ambiguous_jsx_like: true,
          dts: false,
          no_early_errors: true,
          tsx: false,
        },
        mode: swc_ts_fast_strip::Mode::StripOnly,
        transform: None,
        deprecated_ts_module_as_error: Some(true),
        source_map: false,
      },
    )
  })?;

  let mut diagnostics = diagnostics_cell.borrow_mut();
  let diagnostics = std::mem::take(&mut *diagnostics);
  ensure_no_fatal_swc_diagnostics(&source_map, diagnostics.into_iter())?;
  Ok(output.code)
}
