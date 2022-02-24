// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./helpers/mod.ts";

const repos = new Repos();

// Ensure repos are latest main
for (const repo of repos.nonDenoAstRepos()) {
  console.log(`Setting up ${repo.name}...`);
  if (await repo.hasLocalChanges()) {
    throw new Error(
      `Repo ${repo.name} had local changes. Please resolve this.`,
    );
  }
  console.log(`  Switching to main...`);
  await repo.switchMain();
  console.log(`  Pulling upstream main...`);
  await repo.pullUpstreamMain();
}

// Update the repos to refer to local versions of each other
await repos.toLocalSource();
