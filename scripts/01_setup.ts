import { Crates } from "./helpers/mod.ts";

const crates = new Crates();
const deno_ast = crates.get("deno_ast");
const deno_graph = crates.get("deno_graph");
const deno_doc = crates.get("deno_doc");
const deno_lint = crates.get("deno_lint");
const dprint_plugin_typescript = crates.get("dprint-plugin-typescript");
const deno = crates.get("deno");

// Ensure repos are latest main
for (const crate of [deno_graph, deno_doc, deno_lint, dprint_plugin_typescript, deno]) {
  console.log(`Setting up ${crate.name}...`)
  if (await crate.hasLocalChanges()) {
    throw new Error(`Repo ${crate.name} had local changes. Please resolve this.`)
  }
  console.log(`  Switching to main...`);
  await crate.switchMain();
  console.log(`  Pulling upstream main...`);
  await crate.pullUpstreamMain();
}

// Update the repos to refer to local versions of each other
await deno_graph.localSourceDependency(deno_ast);

await deno_doc.localSourceDependency(deno_ast);
await deno_doc.localSourceDependency(deno_graph);

await deno_lint.localSourceDependency(deno_ast);

await dprint_plugin_typescript.localSourceDependency(deno_ast);

await deno.localSourceDependency(deno_ast);
await deno.localSourceDependency(deno_graph);
await deno.localSourceDependency(deno_doc);
await deno.localSourceDependency(deno_lint);
await deno.localSourceDependency(dprint_plugin_typescript);
