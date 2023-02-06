// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";

const repos = await Repos.load({ skipLoadingCrates: true });

if (confirm("Are you sure you want to git reset --hard all the repos?")) {
  await Promise.all(repos.nonDenoAstRepos().map((c) => c.gitResetHard()));
}
