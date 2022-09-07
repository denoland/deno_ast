# Scripts

These scripts provide a way to help upgrade, test, and open PRs in downstream
crates on an swc version bump.

## Setup

1. Ensure all repos are cloned into sibling directories:

- `./deno`
- `./deno_ast`
- `./deno_doc`
- `./deno_emit`
- `./deno_graph`
- `./deno_lint`
- `./dprint-plugin-typescript`

2. Ensure all repos have an `upstream` remote defined as the original repo.

## Overview

- `01_setup.ts` - Ensures all downstream crates are on the latest main, then
  points them at local copies of each other.
- `02_build.ts` - Builds each crate. If you encounter any build errors, fix them
  and keep running this until everything builds.
- `03_test.ts` - Tests each crate. If you encounter test failures, fix them and
  keep running this until all the tests pass.
- `04_branch.ts` - Updates the dependency versions, creates a branch, commits,
  and pushes a branch for every selected repo.
