// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Crates } from "./helpers/mod.ts";

const crates = new Crates();

if (confirm("Are you sure you want to git reset --hard all the repos?")) {
  await Promise.all(
    crates.nonDenoAstCrates()
      .map((c) => c.resetHard()),
  );
}
