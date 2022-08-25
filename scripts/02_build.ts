// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";
import { $ } from "./deps.ts";

const repos = await Repos.load();

for (const crate of repos.getCrates()) {
  $.logStep(`Building ${crate.name}...`);
  await crate.build({ allFeatures: true });
}
