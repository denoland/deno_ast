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

## Versioning Strategy

This crate does not follow semver so make sure to pin it to a patch version.
Instead a versioning strategy that optimizes for more efficient maintenance is
used:

- Does [deno_graph](https://github.com/denoland/deno_graph),
  [deno_doc](https://github.com/denoland/deno_doc),
  [deno_lint](https://github.com/denoland/deno_lint),
  [eszip](https://github.com/denoland/eszip), and
  [dprint-plugin-typescript](https://github.com/dprint/dprint-plugin-typescript)
  still compile in the [Deno](https://github.com/denoland/deno) repo?
  - If yes, is this a change that would break something at runtime?
    - If yes, it's recommended to do a minor release.
    - If no, it's a patch release.
  - If no, it's a minor release.

## swc upgrades

We upgrade swc about once a month. Upgrading swc is a very involved process that
often requires many changes in downstream Deno crates. We also test the new
version of swc in all downstream crates before merging a PR into deno_ast that
updates swc.

**Please do not open a PR for upgrading swc** unless you have stated you are
going to work on it in the issue tracker and have run the tests on all
downstream crates.

To upgrade swc:

1. Checkout the following repositories in sibling directories to this repository
   (ex. `/home/david/dev/deno_graph`, `/home/david/dev/deno_ast`):
   - https://github.com/denoland/deno_graph
   - https://github.com/denoland/deno_lint
   - https://github.com/denoland/eszip
   - https://github.com/denoland/deno_doc
   - https://github.com/dprint/dprint-plugin-typescript
   - https://github.com/denoland/deno
1. Ensure they all have `upstream` remotes set for the urls specified above. For
   example, your `deno_graph` repo should have
   `git remote add upstream https://github.com/denoland/deno_graph`.
1. Run `./scripts/update_swc_deps.ts`
1. Run `./scripts/01_setup.ts`
1. Run `./scripts/02_build.ts` and fix any build errors.
1. Run `./scripts/03_test.ts` and fix any failing tests.
   - At this point, all the tests should be passing in all the repositories.
1. Open a PR for upgrading deno_ast.
1. Merge the PR and publish a new version of deno_ast using the
   [release workflow](https://github.com/denoland/deno_ast/actions/workflows/release.yml).
1. At this point, bump the version of deno_ast and open PRs for deno_graph,
   deno_lint, and dprint-plugin-typescript (note: `./scripts/04_confirm.ts`
   might be helpful to automate some of this. Read its source code to understand
   it so that you can deal with any problems that may arise).
1. Merge the PR deno_graph and publish using its release workflow.
1. Open PRs to deno_doc and eszip.
1. Merge those PRs and do releases. Also merge and release deno_lint and
   dprint-plugin-typescript (Bartek and David have access to publish
   dprint-plugin-typescript).
1. Open a PR to Deno with the versions bumped and merge the PR.
