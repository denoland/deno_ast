// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { Crate, Crates } from "./helpers/mod.ts";

const crates = new Crates();
const deno = crates.get("deno");
const deno_ast = crates.get("deno_ast");
const nonDenoCrates = [
  deno_ast,
  crates.get("deno_graph"),
  crates.get("deno_doc"),
  crates.get("deno_lint"),
  crates.get("dprint-plugin-typescript"),
];

// bump the version numbers
for (const crate of nonDenoCrates) {
  await incrementVersion(crate);
}

// set the dependencies back to not being pointed at local copies
await crates.revertLocalSource();

// create a branch, commit, push for the non-deno repos
for (const crate of nonDenoCrates) {
  await crate.branch("release:" + crate.version.toString());
  await crate.commit(crate.version.toString());
  await crate.push();
}

// now branch, commit, and push for the deno repo
await deno.branch("deno_ast:" + deno_ast.version.toString());
await deno.commit(`chore: upgrade to deno_ast ${deno_ast.version.toString()}`);
await deno.push();

async function incrementVersion(crate: Crate) {
  console.log(`${crate.name} is on ${crate.version}`);
  await crate.setVersion(crate.version.inc(getVersionIncrement()));
  console.log(`Set to ${crate.version}`);
}

function getVersionIncrement() {
  if (confirm("Increment patch?")) {
    return "patch";
  } else if (confirm("Increment minor?")) {
    return "minor";
  } else if (confirm("Increment major?")) {
    return "major";
  } else {
    throw new Error("No decision.");
  }
}
