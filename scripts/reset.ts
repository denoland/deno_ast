import { Crates } from "./helpers/mod.ts";

const crates = new Crates();
const deno_graph = crates.get("deno_graph");
const deno_doc = crates.get("deno_doc");
const deno_lint = crates.get("deno_lint");
const deno = crates.get("deno");

if (confirm("Are you sure you want to git reset --hard all the repos?")) {
  await Promise.all([deno_graph, deno_doc, deno_lint, deno].map(c => c.resetHard()));
}
