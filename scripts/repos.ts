// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.

import { $, Crate, Repo } from "./deps.ts";

export const rootDir = $.path(import.meta).join("../../../").resolve();

const repoNames = [
  "deno_ast",
  "deno_graph",
  "deno_lint",
  "dprint-plugin-typescript",
  "deno_doc",
  "eszip",
  "deno_emit",
  "deno",
];

export class Repos {
  #repos: readonly Repo[];

  private constructor(repos: readonly Repo[]) {
    this.#repos = repos;
  }

  static createWithoutLoading() {
    return;
  }

  static async load({ skipLoadingCrates = false } = {}) {
    if (!skipLoadingCrates) {
      $.logStep("Loading repos...");
    }
    const repos = [];
    for (const repoName of repoNames) {
      $.logStep("Loading", repoName);
      repos.push(await loadRepo(repoName));
    }
    return new Repos(repos);

    function loadRepo(name: string) {
      return Repo.load({
        name,
        path: rootDir.join(name),
        skipLoadingCrates,
      }).catch((err) => {
        console.error(`Error loading: ${name}`);
        throw err;
      });
    }
  }

  getRepos() {
    return [...this.#repos];
  }

  getCrates() {
    const crates = [];
    for (const repo of this.#repos) {
      if (repo.name === "deno") {
        crates.push(repo.getCrate("deno"));
      } else {
        crates.push(
          ...repo.crates.filter((c) =>
            c.name !== "eszip_wasm" && c.name !== "deno_emit_wasm"
          ),
        );
      }
    }
    return crates;
  }

  nonDenoAstRepos() {
    return this.#repos.filter((c) => c.name !== "deno_ast");
  }

  get(name: string) {
    const repo = this.#repos.find((c) => c.name === name);
    if (repo == null) {
      throw new Error(`Could not find repo with name ${name}.`);
    }
    return repo;
  }

  getCrate(name: string) {
    for (const repo of this.#repos) {
      for (const crate of repo.crates) {
        if (crate.name === name) {
          return crate;
        }
      }
    }

    throw new Error(`Could not find crate: ${name}`);
  }

  async toLocalSource() {
    for (
      const [workingCrate, otherCrate] of this.#getLocalSourceRelationships()
    ) {
      await workingCrate.toLocalSource(otherCrate);
    }
  }

  async revertLocalSource() {
    for (
      const [workingCrate, depCrate] of this.#getLocalSourceRelationships()
    ) {
      await workingCrate.revertLocalSource(depCrate);
    }
  }

  getCrateLocalSourceCrates(crate: Crate) {
    return this.#getLocalSourceRelationships()
      .filter(([workingCrate]) => workingCrate === crate)
      .map(([_workingCrate, depCrate]) => depCrate);
  }

  #getLocalSourceRelationships() {
    const deno_ast = this.getCrate("deno_ast");
    const deno_graph = this.getCrate("deno_graph");
    const deno_doc = this.getCrate("deno_doc");
    const deno_lint = this.getCrate("deno_lint");
    const dprint_plugin_typescript = this.getCrate("dprint-plugin-typescript");
    const deno_cli = this.getCrate("deno");
    const eszip = this.getCrate("eszip");
    const deno_emit = this.getCrate("deno_emit");

    return [
      [deno_graph, deno_ast],
      [deno_doc, deno_ast],
      [deno_doc, deno_graph],
      [eszip, deno_ast],
      [eszip, deno_graph],
      [deno_lint, deno_ast],
      [dprint_plugin_typescript, deno_ast],
      [deno_emit, deno_graph],
      [deno_emit, deno_ast],
      [deno_cli, deno_ast],
      [deno_cli, deno_graph],
      [deno_cli, deno_doc],
      [deno_cli, deno_lint],
      [deno_cli, eszip],
      [deno_cli, dprint_plugin_typescript],
      [deno_cli, deno_emit],
    ] as [Crate, Crate][];
  }
}
