// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

import { $, Repo } from "./deps.ts";
import { Repos } from "./repos.ts";

const repos = await Repos.load();
const denoRepo = repos.get("deno");
const deno_ast = repos.getCrate("deno_ast");
const nonDenoRepos = repos.getRepos().filter((c) => c.name !== "deno");

// create a branch, commit, push for the non-deno repos
for (const repo of nonDenoRepos) {
  if (!await repo.hasLocalChanges()) {
    continue;
  }
  const currentBranch = await repo.gitCurrentBranch();
  $.logStep("Analyzing", repo.name);
  $.logLight("Branch:", currentBranch);
  if (
    confirm(
      `Bump deps? (Note: do this after the dependency crates have PUBLISHED)`,
    )
  ) {
    await bumpDeps(repo);
    for (const crate of repo.crates) {
      await crate.cargoCheck();
    }

    if (
      currentBranch === "main" &&
      confirm(`Branch for ${repo.name}?`)
    ) {
      await repo.gitBranch("deno_ast_" + deno_ast.version);
    }
    if (
      await repo.hasLocalChanges() &&
      confirm(`Commit and push for ${repo.name}?`)
    ) {
      await repo.gitAdd();
      await repo.gitCommit(`feat: upgrade deno_ast to ${deno_ast.version}`);
      await repo.gitPush();
    }
  }
}

// now branch, commit, and push for the deno repo
$.logStep("Analyzing Deno");
const currentBranch = await denoRepo.gitCurrentBranch();
$.logLight("Branch:", currentBranch);
if (confirm(`Bump deps for deno?`)) {
  await bumpDeps(denoRepo);
  for (const crate of denoRepo.crates) {
    await crate.cargoCheck();
  }
  if (
    currentBranch === "main" &&
    confirm(`Branch for deno?`)
  ) {
    await denoRepo.gitBranch("deno_ast_" + deno_ast.version);
  }
  if (
    await denoRepo.hasLocalChanges() && confirm(`Commit and push for deno?`)
  ) {
    await denoRepo.gitAdd();
    await denoRepo.gitCommit(
      `chore: upgrade to deno_ast ${deno_ast.version}`,
    );
    await denoRepo.gitPush();
  }
}

async function bumpDeps(repo: Repo) {
  for (const crate of repo.crates) {
    for (const depCrate of repos.getCrateLocalSourceCrates(crate)) {
      await crate.revertLocalSource(depCrate);
      const version = await depCrate.getLatestVersion();
      if (version == null) {
        throw new Error(`Did not find version for ${crate.name}`);
      }
      await crate.setDependencyVersion(depCrate.name, version);
    }
  }
}
