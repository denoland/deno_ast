// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::swc::common::sync::Lrc;
use crate::swc::common::SourceFile;

use crate::ModuleSpecifier;

#[derive(Clone, Default)]
pub struct SourceMap {
  inner: Lrc<crate::swc::common::SourceMap>,
}

impl SourceMap {
  pub fn single(specifier: ModuleSpecifier, source: String) -> Self {
    let map = Self::default();
    map
      .inner
      .new_source_file(Lrc::new(swc_common::FileName::Url(specifier)), source);
    map
  }

  pub fn inner(&self) -> &Lrc<crate::swc::common::SourceMap> {
    &self.inner
  }

  pub fn new_source_file(
    &self,
    specifier: ModuleSpecifier,
    source: String,
  ) -> Lrc<SourceFile> {
    self
      .inner
      .new_source_file(Lrc::new(swc_common::FileName::Url(specifier)), source)
  }
}
