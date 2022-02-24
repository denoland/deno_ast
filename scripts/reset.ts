// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./helpers/mod.ts";

const repos = new Repos();

if (confirm("Are you sure you want to git reset --hard all the repos?")) {
  await Promise.all(
    repos.nonDenoAstRepos()
      .map((c) => c.resetHard()),
  );
}
