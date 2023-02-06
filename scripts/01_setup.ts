// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";
import { $ } from "./deps.ts";

const repos = await Repos.load();

// Ensure repos are latest main
for (const repo of repos.nonDenoAstRepos()) {
  $.logStep("Setting up", `${repo.name}...`);
  if (await repo.hasLocalChanges()) {
    throw new Error(
      `Repo ${repo.name} had local changes. Please resolve this.`,
    );
  }
  $.logGroup();
  $.logStep("Switching to main...");
  await repo.command("git switch main");
  $.logStep("Pulling upstream main...");
  await repo.command("git pull upstream main");
  $.logGroupEnd();
}

// Update the repos to refer to local versions of each other
await repos.toLocalSource();
