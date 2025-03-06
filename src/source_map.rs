// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

use crate::swc::common::sync::Lrc;
use crate::swc::common::FileName;
use crate::swc::common::SourceFile;
use crate::ModuleSpecifier;

// this is used in JSR, so don't remove it
pub trait IntoSwcFileName {
  fn into_file_name(self) -> FileName;
}

impl IntoSwcFileName for ModuleSpecifier {
  fn into_file_name(self) -> FileName {
    FileName::Url(self)
  }
}

impl IntoSwcFileName for String {
  fn into_file_name(self) -> FileName {
    FileName::Custom(self)
  }
}

#[derive(Clone, Default)]
pub struct SourceMap {
  inner: Lrc<crate::swc::common::SourceMap>,
}

impl SourceMap {
  pub fn single(file_name: impl IntoSwcFileName, source: String) -> Self {
    let map = Self::default();
    map.inner.new_source_file(
      Lrc::new(IntoSwcFileName::into_file_name(file_name)),
      source,
    );
    map
  }

  pub fn inner(&self) -> &Lrc<crate::swc::common::SourceMap> {
    &self.inner
  }

  pub fn new_source_file(
    &self,
    file_name: impl IntoSwcFileName,
    source: String,
  ) -> Lrc<SourceFile> {
    self.inner.new_source_file(
      Lrc::new(IntoSwcFileName::into_file_name(file_name)),
      source,
    )
  }
}
