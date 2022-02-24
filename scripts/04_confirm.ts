// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Crate, Repos } from "./helpers/mod.ts";

const repos = new Repos();
const nonDenoCrates = repos.getCrates().filter((c) => c.name !== "deno");

// bump the version numbers
for (const crate of nonDenoCrates) {
  await incrementVersion(crate);
}

// set the dependencies back to not being pointed at local copies
await repos.revertLocalSource();

async function incrementVersion(crate: Crate) {
  console.log(`${crate.name} is on ${crate.version}`);
  const versionIncrement = getVersionIncrement();
  if (versionIncrement != null) {
    await crate.setVersion(crate.version.inc(versionIncrement));
    console.log(`Set to ${crate.version}`);
  }
}

function getVersionIncrement() {
  if (confirm("Increment patch?")) {
    return "patch";
  } else if (confirm("Increment minor?")) {
    return "minor";
  } else if (confirm("Increment major?")) {
    return "major";
  } else {
    return undefined;
  }
}
