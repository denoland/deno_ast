// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repo, Repos } from "./helpers/mod.ts";

const repos = new Repos();
const denoRepo = repos.get("deno");
const deno_ast = repos.getCrate("deno_ast");
const nonDenoRepos = repos.getRepos().filter((c) => c.name !== "deno");

// create a branch, commit, push for the non-deno repos
for (const repo of nonDenoRepos) {
  if (confirm(`Branch for ${repo.name}?`)) {
    await preAction(repo);
    const version = repo.getCrates()[0].version;
    for (const crate of repo.getCrates()) {
      await crate.cargoCheck();
    }
    await repo.branch("release_" + version.toString());
    await repo.gitAdd();
    await repo.commit(version.toString());
    await repo.push();
  }
}

// now branch, commit, and push for the deno repo
if (confirm(`Branch for deno?`)) {
  for (const crate of denoRepo.getCrates()) {
    await crate.cargoCheck();
  }
  await denoRepo.branch("deno_ast_" + deno_ast.version.toString());
  await denoRepo.gitAdd();
  await denoRepo.commit(
    `chore: upgrade to deno_ast ${deno_ast.version.toString()}`,
  );
  await denoRepo.push();
}

async function preAction(repo: Repo) {
  switch (repo.name) {
    case "deno_graph":
    case "deno_doc":
      await repo.runCommandWithOutput([
        "deno",
        "run",
        "-A",
        "--unstable",
        "_build.ts",
      ]);
      break;
    default:
      break;
  }
}
