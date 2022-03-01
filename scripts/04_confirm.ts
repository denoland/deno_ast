// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { Repos } from "./repos.ts";

const repos = await Repos.load();

const nonDenoCrates = repos.getCrates().filter((c) => c.name !== "deno");

// bump the version numbers
for (const crate of nonDenoCrates) {
  await crate.promptAndTryIncrement();
}

// set the dependencies back to not being pointed at local copies
await repos.revertLocalSource();
