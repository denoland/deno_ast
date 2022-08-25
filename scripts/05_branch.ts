// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repo } from "./deps.ts";
import { Repos } from "./repos.ts";

const repos = await Repos.load({
  // Delay loading crates until they're ready to be branched because
  // they may have dependencies that aren't published yet.
  skipLoadingCrates: true,
});
const denoRepo = repos.get("deno");
await repos.get("deno_ast").loadCrates();
const deno_ast = repos.getCrate("deno_ast");
const nonDenoRepos = repos.getRepos().filter((c) => c.name !== "deno");

// create a branch, commit, push for the non-deno repos
for (const repo of nonDenoRepos) {
  if (confirm(`Branch for ${repo.name}?`)) {
    await preAction(repo);
    if (repo.name !== "deno_ast") {
      await repo.loadCrates();
    }
    const version = repo.crates[0].version;
    for (const crate of repo.crates) {
      await crate.cargoCheck();
    }
    await repo.gitBranch("release_" + version.toString());
    await repo.gitAdd();
    await repo.gitCommit(version.toString());
    await repo.gitPush();
  }
}

// now branch, commit, and push for the deno repo
if (confirm(`Branch for deno?`)) {
  for (const crate of denoRepo.crates) {
    await crate.cargoCheck();
  }
  await denoRepo.gitBranch("deno_ast_" + deno_ast.version.toString());
  await denoRepo.gitAdd();
  await denoRepo.gitCommit(
    `chore: upgrade to deno_ast ${deno_ast.version.toString()}`,
  );
  await denoRepo.gitPush();
}

async function preAction(repo: Repo) {
  switch (repo.name) {
    case "deno_graph":
    case "deno_doc":
      await repo.command("deno task build");
      break;
    default:
      break;
  }
}
