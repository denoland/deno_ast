// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { Crates } from "./helpers/mod.ts";

const crates = new Crates();
const deno = crates.get("deno");
const deno_ast = crates.get("deno_ast");
const nonDenoCrates = crates.crates.filter((c) => c.name !== "deno");

// create a branch, commit, push for the non-deno repos
for (const crate of nonDenoCrates) {
  if (confirm(`Branch for ${crate.name}?`)) {
    await crate.cargoCheck();
    await crate.branch("release_" + crate.version.toString());
    await crate.gitAdd();
    await crate.commit(crate.version.toString());
    await crate.push();
  }
}

// now branch, commit, and push for the deno repo
if (confirm(`Branch for deno?`)) {
  await deno.cargoCheck();
  await deno.branch("deno_ast_" + deno_ast.version.toString());
  await deno.gitAdd();
  await deno.commit(
    `chore: upgrade to deno_ast ${deno_ast.version.toString()}`,
  );
  await deno.push();
}
