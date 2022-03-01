// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";

const repos = await Repos.load();

// Ensure repos are latest main
for (const repo of repos.nonDenoAstRepos()) {
  console.log(`Setting up ${repo.name}...`);
  if (await repo.hasLocalChanges()) {
    throw new Error(
      `Repo ${repo.name} had local changes. Please resolve this.`,
    );
  }
  console.log(`  Switching to main...`);
  await repo.gitSwitchMain();
  console.log(`  Pulling upstream main...`);
  await repo.gitPullMain("upstream");
}

// Update the repos to refer to local versions of each other
await repos.toLocalSource();
