// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { $ } from "./deps.ts";
import { Repos } from "./repos.ts";

const repos = await Repos.load();
let hadConfirmed = false;

for (const crate of repos.getCrates()) {
  if (hadConfirmed || confirm(`Do you want to run tests for ${crate.name}?`)) {
    hadConfirmed = true;
    $.logStep("Running tests", `for ${crate.name}...`);
    await crate.test({ allFeatures: true });
  }
}
