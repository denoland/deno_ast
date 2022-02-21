// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

import { Crates } from "./helpers/mod.ts";

const crates = new Crates();

// Ensure repos are latest main
for (
  const crate of crates.nonDenoAstCrates()
) {
  console.log(`Setting up ${crate.name}...`);
  if (await crate.hasLocalChanges()) {
    throw new Error(
      `Repo ${crate.name} had local changes. Please resolve this.`,
    );
  }
  console.log(`  Switching to main...`);
  await crate.switchMain();
  console.log(`  Pulling upstream main...`);
  await crate.pullUpstreamMain();
}

// Update the repos to refer to local versions of each other
await crates.toLocalSource();
