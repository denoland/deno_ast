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

let source_text = "class MyClass {}";
let text_info = SourceTextInfo::new(source_text.into());
let parsed_source = parse_module(ParseParams {
  specifier: deno_ast::ModuleSpecifier::parse("file:///my_file.ts").unwrap(),
  media_type: MediaType::TypeScript,
  text_info,
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
parsed_source.text_info();
```
