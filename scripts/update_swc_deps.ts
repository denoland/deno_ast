// Copyright 2018-2023 the Deno authors. All rights reserved. MIT license.

import { $, Crate, Repo } from "./deps.ts";

const repo = await Repo.load({
  name: "deno_ast",
  path: $.path(import.meta).parentOrThrow().parentOrThrow().resolve(),
});

const crate = repo.getCrate("deno_ast");
const swcDeps = crate.dependencies.filter((dep) =>
  dep.name.startsWith("swc_") || dep.name === "dprint-swc-ext"
);
for (const dep of swcDeps) {
  const version = await Crate.getLatestVersion(dep.name);
  if (version == null) {
    throw new Error(`Could not find latest version for ${dep.name}`);
  }

  const newReq = dep.name === "dprint-swc-ext" ? "^" + version : "=" + version;
  if (newReq !== dep.req) {
    $.logStep("Updating", dep.name, "from", dep.req, "to", newReq);
    crate.setDependencyVersion(dep.name, newReq.replace(/^\^/, ""));
  }
}
