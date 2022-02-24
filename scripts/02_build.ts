// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./helpers/mod.ts";

const repos = new Repos();

for (const crate of repos.getCrates()) {
  if (crate.name === "eszip_wasm") {
    continue;
  }

  console.log(`Building ${crate.name}...`);
  await crate.build();
}
