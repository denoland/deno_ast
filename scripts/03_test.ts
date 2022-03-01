// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";

const repos = await Repos.load();
let hadConfirmed = false;

for (const crate of repos.getCrates()) {
  if (crate.name === "eszip_wasm") {
    continue;
  }

  if (hadConfirmed || confirm(`Do you want to run tests for ${crate.name}?`)) {
    hadConfirmed = true;
    console.log(`Running tests for ${crate.name}...`);
    await crate.test({ allFeatures: true });
  }
}
