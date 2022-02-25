// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Crates } from "./helpers/mod.ts";

const crates = new Crates();

for (const crate of crates.crates) {
  console.log(`Building ${crate.name}...`);
  await crate.build();
}
