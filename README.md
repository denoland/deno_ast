# deno_ast

[![](https://img.shields.io/crates/v/deno_ast.svg)](https://crates.io/crates/deno_ast)
[![Discord Chat](https://img.shields.io/discord/684898665143206084?logo=discord&style=social)](https://discord.gg/deno)

Source text parsing, lexing, and AST related functionality for
[Deno](https://deno.land).

```rust
use deno_ast::parse_module;
use deno_ast::MediaType;
use deno_ast::ParseParams;
use deno_ast::SourceTextInfo;
use std::sync::Arc;

let source_text = Arc::new("class MyClass {}");
let source_text_info = SourceTextInfo::new(source_text);
let parsed_source = parse_module(ParseParams {
  specifier: "file:///my_file.ts".to_string(),
  media_type: MediaType::TypeScript,
  source: source_text_info,
  capture_tokens: true,
  maybe_syntax: None,
  scope_analysis: false,
}).expect("should parse");

// returns the comments
parsed_source.comments();
// returns the tokens if captured
parsed_source.tokens();
// returns the module (AST)
parsed_source.module();
// returns the `SourceTextInfo`
parsed_source.source();
```
