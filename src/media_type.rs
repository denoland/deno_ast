// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

use serde::Serialize;
use serde::Serializer;
use std::fmt;
use std::path::Path;
use std::path::PathBuf;

#[cfg(feature = "module_specifier")]
pub type ModuleSpecifier = url::Url;

#[repr(i32)]
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

impl MediaType {
  /// Convert a MediaType to a `ts.Extension`.
  ///
  /// *NOTE* This is defined in TypeScript as a string based enum.  Changes to
  /// that enum in TypeScript should be reflected here.
  pub fn as_ts_extension(&self) -> &str {
    match self {
      Self::JavaScript => ".js",
      Self::Jsx => ".jsx",
      Self::TypeScript => ".ts",
      Self::Dts => ".d.ts",
      Self::Tsx => ".tsx",
      Self::Json => ".json",
      // TypeScript doesn't have an "unknown", so we will treat WASM as JS for
      // mapping purposes, though in reality, it is unlikely to ever be passed
      // to the compiler.
      Self::Wasm => ".js",
      Self::TsBuildInfo => ".tsbuildinfo",
      // TypeScript doesn't have an "source map", so we will treat SourceMap as
      // JS for mapping purposes, though in reality, it is unlikely to ever be
      // passed to the compiler.
      Self::SourceMap => ".js",
      // TypeScript doesn't have an "unknown", so we will treat unknowns as JS
      // for mapping purposes, though in reality, it is unlikely to ever be
      // passed to the compiler.
      Self::Unknown => ".js",
    }
  }

  /// Map the media type to a `ts.ScriptKind`
  pub fn as_ts_script_kind(&self) -> i32 {
    match self {
      Self::JavaScript => 1,
      Self::Jsx => 2,
      Self::TypeScript => 3,
      Self::Dts => 3,
      Self::Tsx => 4,
      Self::Json => 5,
      _ => 0,
    }
  }

  #[cfg(feature = "module_specifier")]
  pub fn from_content_type<S: AsRef<str>>(
    specifier: &ModuleSpecifier,
    content_type: S,
  ) -> Self {
    match content_type.as_ref().split(';').collect::<Vec<&str>>()[0]
      .trim()
      .to_lowercase()
      .as_ref()
    {
      "application/typescript"
      | "text/typescript"
      | "video/vnd.dlna.mpeg-tts"
      | "video/mp2t"
      | "application/x-typescript" => {
        map_js_like_extension(specifier, Self::TypeScript)
      }
      "application/javascript"
      | "text/javascript"
      | "application/ecmascript"
      | "text/ecmascript"
      | "application/x-javascript"
      | "application/node" => {
        map_js_like_extension(specifier, Self::JavaScript)
      }
      "text/jsx" => Self::Jsx,
      "text/tsx" => Self::Tsx,
      "application/json" | "text/json" => Self::Json,
      "application/wasm" => Self::Wasm,
      // Handle plain and possibly webassembly
      "text/plain" | "application/octet-stream"
        if specifier.scheme() != "data" =>
      {
        Self::from(specifier)
      }
      _ => Self::Unknown,
    }
  }

  pub fn from_path(path: &Path) -> Self {
    match path.extension() {
      None => match path.file_name() {
        None => Self::Unknown,
        Some(os_str) => match os_str.to_str() {
          Some(".tsbuildinfo") => Self::TsBuildInfo,
          _ => Self::Unknown,
        },
      },
      Some(os_str) => match os_str.to_str() {
        Some("ts") => {
          if let Some(os_str) = path.file_stem() {
            if let Some(file_name) = os_str.to_str() {
              if file_name.ends_with(".d") {
                return Self::Dts;
              }
            }
          }
          Self::TypeScript
        }
        Some("tsx") => Self::Tsx,
        Some("js") => Self::JavaScript,
        Some("jsx") => Self::Jsx,
        Some("mjs") => Self::JavaScript,
        Some("cjs") => Self::JavaScript,
        Some("json") => Self::Json,
        Some("wasm") => Self::Wasm,
        Some("tsbuildinfo") => Self::TsBuildInfo,
        Some("map") => Self::SourceMap,
        _ => Self::Unknown,
      },
    }
  }
}

impl Default for MediaType {
  fn default() -> Self {
    Self::Unknown
  }
}

impl Serialize for MediaType {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: Serializer,
  {
    Serialize::serialize(&self.to_string(), serializer)
  }
}

impl fmt::Display for MediaType {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    let value = match self {
      Self::JavaScript => "JavaScript",
      Self::Jsx => "JSX",
      Self::TypeScript => "TypeScript",
      Self::Dts => "Dts",
      Self::Tsx => "TSX",
      Self::Json => "Json",
      Self::Wasm => "Wasm",
      Self::TsBuildInfo => "TsBuildInfo",
      Self::SourceMap => "SourceMap",
      Self::Unknown => "Unknown",
    };
    write!(f, "{}", value)
  }
}

impl<'a> From<&'a Path> for MediaType {
  fn from(path: &'a Path) -> Self {
    Self::from_path(path)
  }
}

impl<'a> From<&'a PathBuf> for MediaType {
  fn from(path: &'a PathBuf) -> Self {
    Self::from_path(path)
  }
}

impl<'a> From<&'a String> for MediaType {
  fn from(specifier: &'a String) -> Self {
    Self::from_path(&PathBuf::from(specifier))
  }
}

#[cfg(feature = "module_specifier")]
#[cfg(not(target_arch = "wasm32"))]
fn file_specifier_to_path(specifier: &ModuleSpecifier) -> PathBuf {
  if let Ok(path) = specifier.to_file_path() {
    path
  } else {
    PathBuf::from(specifier.path())
  }
}

#[cfg(feature = "module_specifier")]
#[cfg(target_arch = "wasm32")]
fn file_specifier_to_path(specifier: &ModuleSpecifier) -> PathBuf {
  PathBuf::from(specifier.path())
}

#[cfg(feature = "module_specifier")]
impl<'a> From<&'a ModuleSpecifier> for MediaType {
  fn from(specifier: &'a ModuleSpecifier) -> Self {
    use data_url::DataUrl;

    if specifier.scheme() != "data" {
      let path = if specifier.scheme() == "file" {
        file_specifier_to_path(specifier)
      } else {
        PathBuf::from(specifier.path())
      };
      Self::from_path(&path)
    } else if let Ok(data_url) = DataUrl::process(specifier.as_str()) {
      Self::from_content_type(specifier, data_url.mime_type().to_string())
    } else {
      Self::Unknown
    }
  }
}

/// Used to augment media types by using the path part of a module specifier to
/// resolve to a more accurate media type.
#[cfg(feature = "module_specifier")]
fn map_js_like_extension(
  specifier: &ModuleSpecifier,
  default: MediaType,
) -> MediaType {
  let path = if specifier.scheme() == "file" {
    file_specifier_to_path(specifier)
  } else {
    PathBuf::from(specifier.path())
  };
  match path.extension() {
    None => default,
    Some(os_str) => match os_str.to_str() {
      None => default,
      Some("jsx") => MediaType::Jsx,
      Some("tsx") => MediaType::Tsx,
      // Because DTS files do not have a separate media type, or a unique
      // extension, we have to "guess" at those things that we consider that
      // look like TypeScript, and end with `.d.ts` are DTS files.
      Some("ts") => {
        if default == MediaType::TypeScript {
          match path.file_stem() {
            None => default,
            Some(os_str) => {
              if let Some(file_stem) = os_str.to_str() {
                if file_stem.ends_with(".d") {
                  MediaType::Dts
                } else {
                  default
                }
              } else {
                default
              }
            }
          }
        } else {
          default
        }
      }
      Some(_) => default,
    },
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  use serde_json::json;

  /// Normalize all intermediate components of the path (ie. remove "./" and "../" components).
  /// Similar to `fs::canonicalize()` but doesn't resolve symlinks.
  ///
  /// Taken from Cargo
  /// https://github.com/rust-lang/cargo/blob/af307a38c20a753ec60f0ad18be5abed3db3c9ac/src/cargo/util/paths.rs#L60-L85
  #[cfg(feature = "module_specifier")]
  fn normalize_path<P: AsRef<Path>>(path: P) -> PathBuf {
    use std::path::Component;

    let mut components = path.as_ref().components().peekable();
    let mut ret =
      if let Some(c @ Component::Prefix(..)) = components.peek().cloned() {
        components.next();
        PathBuf::from(c.as_os_str())
      } else {
        PathBuf::new()
      };

    for component in components {
      match component {
        Component::Prefix(..) => unreachable!(),
        Component::RootDir => {
          ret.push(component.as_os_str());
        }
        Component::CurDir => {}
        Component::ParentDir => {
          ret.pop();
        }
        Component::Normal(c) => {
          ret.push(c);
        }
      }
    }
    ret
  }

  /// Returns true if the input string starts with a sequence of characters
  /// that could be a valid URI scheme, like 'https:', 'git+ssh:' or 'data:'.
  ///
  /// According to RFC 3986 (https://tools.ietf.org/html/rfc3986#section-3.1),
  /// a valid scheme has the following format:
  ///   scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
  ///
  /// We additionally require the scheme to be at least 2 characters long,
  /// because otherwise a windows path like c:/foo would be treated as a URL,
  /// while no schemes with a one-letter name actually exist.
  #[cfg(feature = "module_specifier")]
  fn specifier_has_uri_scheme(specifier: &str) -> bool {
    let mut chars = specifier.chars();
    let mut len = 0usize;
    // The first character must be a letter.
    match chars.next() {
      Some(c) if c.is_ascii_alphabetic() => len += 1,
      _ => return false,
    }
    // Second and following characters must be either a letter, number,
    // plus sign, minus sign, or dot.
    loop {
      match chars.next() {
        Some(c) if c.is_ascii_alphanumeric() || "+-.".contains(c) => len += 1,
        Some(':') if len >= 2 => return true,
        _ => return false,
      }
    }
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_url(url_str: &str) -> ModuleSpecifier {
    ModuleSpecifier::parse(url_str).expect("Invalid url.")
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_path(path_str: &str) -> ModuleSpecifier {
    let path = std::env::current_dir().unwrap().join(path_str);
    let path = normalize_path(&path);
    ModuleSpecifier::from_file_path(path).expect("Invalid path.")
  }

  #[cfg(feature = "module_specifier")]
  fn resolve_url_or_path(specifier: &str) -> ModuleSpecifier {
    if specifier_has_uri_scheme(specifier) {
      resolve_url(specifier)
    } else {
      resolve_path(specifier)
    }
  }

  #[test]
  fn test_map_file_extension() {
    assert_eq!(
      MediaType::from(Path::new("foo/bar.ts")),
      MediaType::TypeScript
    );
    assert_eq!(MediaType::from(Path::new("foo/bar.tsx")), MediaType::Tsx);
    assert_eq!(MediaType::from(Path::new("foo/bar.d.ts")), MediaType::Dts);
    assert_eq!(
      MediaType::from(Path::new("foo/bar.js")),
      MediaType::JavaScript
    );
    assert_eq!(MediaType::from(Path::new("foo/bar.jsx")), MediaType::Jsx);
    assert_eq!(MediaType::from(Path::new("foo/bar.json")), MediaType::Json);
    assert_eq!(MediaType::from(Path::new("foo/bar.wasm")), MediaType::Wasm);
    assert_eq!(
      MediaType::from(Path::new("foo/bar.cjs")),
      MediaType::JavaScript
    );
    assert_eq!(
      MediaType::from(Path::new("foo/.tsbuildinfo")),
      MediaType::TsBuildInfo
    );
    assert_eq!(
      MediaType::from(Path::new("foo/bar.js.map")),
      MediaType::SourceMap
    );
    assert_eq!(
      MediaType::from(Path::new("foo/bar.txt")),
      MediaType::Unknown
    );
    assert_eq!(MediaType::from(Path::new("foo/bar")), MediaType::Unknown);
  }

  #[cfg(feature = "module_specifier")]
  #[test]
  fn test_from_specifier() {
    let fixtures = vec![
      ("file:///a/b/c.ts", MediaType::TypeScript),
      ("file:///a/b/c.js", MediaType::JavaScript),
      ("file:///a/b/c.txt", MediaType::Unknown),
      ("https://deno.land/x/mod.ts", MediaType::TypeScript),
      ("https://deno.land/x/mod.js", MediaType::JavaScript),
      ("https://deno.land/x/mod.txt", MediaType::Unknown),
      ("data:application/typescript;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::TypeScript),
      ("data:application/javascript;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::JavaScript),
      ("data:text/plain;base64,ZXhwb3J0IGNvbnN0IGEgPSAiYSI7CgpleHBvcnQgZW51bSBBIHsKICBBLAogIEIsCiAgQywKfQo=", MediaType::Unknown),
    ];

    for (specifier, expected) in fixtures {
      let actual = resolve_url_or_path(specifier);
      assert_eq!(MediaType::from(&actual), expected);
    }
  }

  #[cfg(feature = "module_specifier")]
  #[test]
  fn test_from_content_type() {
    let fixtures = vec![
      (
        "https://deno.land/x/mod.ts",
        "application/typescript",
        MediaType::TypeScript,
      ),
      (
        "https://deno.land/x/mod.d.ts",
        "application/typescript",
        MediaType::Dts,
      ),
      ("https://deno.land/x/mod.tsx", "text/tsx", MediaType::Tsx),
      (
        "https://deno.land/x/mod.js",
        "application/javascript",
        MediaType::JavaScript,
      ),
      ("https://deno.land/x/mod.jsx", "text/jsx", MediaType::Jsx),
      (
        "https://deno.land/x/mod.ts",
        "text/plain",
        MediaType::TypeScript,
      ),
      (
        "https://deno.land/x/mod.js",
        "text/plain",
        MediaType::JavaScript,
      ),
      (
        "https://deno.land/x/mod.wasm",
        "text/plain",
        MediaType::Wasm,
      ),
    ];

    for (specifier, content_type, expected) in fixtures {
      let fixture = resolve_url_or_path(specifier);
      assert_eq!(
        MediaType::from_content_type(&fixture, content_type),
        expected
      );
    }
  }

  #[test]
  fn test_serialization() {
    assert_eq!(json!(MediaType::JavaScript), json!("JavaScript"));
    assert_eq!(json!(MediaType::Jsx), json!("JSX"));
    assert_eq!(json!(MediaType::TypeScript), json!("TypeScript"));
    assert_eq!(json!(MediaType::Dts), json!("Dts"));
    assert_eq!(json!(MediaType::Tsx), json!("TSX"));
    assert_eq!(json!(MediaType::Json), json!("Json"));
    assert_eq!(json!(MediaType::Wasm), json!("Wasm"));
    assert_eq!(json!(MediaType::TsBuildInfo), json!("TsBuildInfo"));
    assert_eq!(json!(MediaType::SourceMap), json!("SourceMap"));
    assert_eq!(json!(MediaType::Unknown), json!("Unknown"));
  }

  #[test]
  fn test_display() {
    assert_eq!(MediaType::JavaScript.to_string(), "JavaScript");
    assert_eq!(MediaType::Jsx.to_string(), "JSX");
    assert_eq!(MediaType::TypeScript.to_string(), "TypeScript");
    assert_eq!(MediaType::Dts.to_string(), "Dts");
    assert_eq!(MediaType::Tsx.to_string(), "TSX");
    assert_eq!(MediaType::Json.to_string(), "Json");
    assert_eq!(MediaType::Wasm.to_string(), "Wasm");
    assert_eq!(MediaType::TsBuildInfo.to_string(), "TsBuildInfo");
    assert_eq!(MediaType::SourceMap.to_string(), "SourceMap");
    assert_eq!(MediaType::Unknown.to_string(), "Unknown");
  }
}
