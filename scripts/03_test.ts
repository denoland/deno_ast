// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Crates } from "./helpers/mod.ts";

const crates = new Crates();
let hadConfirmed = false;

for (const crate of crates.crates) {
  if (hadConfirmed || confirm(`Do you want to run tests for ${crate.name}?`)) {
    hadConfirmed = true;
    console.log(`Running tests for ${crate.name}...`);
    await crate.test();
  }
}
