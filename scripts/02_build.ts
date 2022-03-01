// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";

const repos = await Repos.load();

for (const crate of repos.getCrates()) {
  if (crate.name === "eszip_wasm") {
    continue;
  }

  console.log(`Building ${crate.name}...`);
  await crate.build({ allFeatures: true });
}
