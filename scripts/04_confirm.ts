// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { Crate, Crates } from "./helpers/mod.ts";

const crates = new Crates();
const nonDenoCrates = crates.crates.filter((c) => c.name !== "deno");

// bump the version numbers
for (const crate of nonDenoCrates) {
  await incrementVersion(crate);
}

// set the dependencies back to not being pointed at local copies
await crates.revertLocalSource();

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
