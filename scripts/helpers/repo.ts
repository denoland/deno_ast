// Copyright 2018-2022 the Deno authors. All rights reserved. MIT license.

import { path, semver } from "./deps.ts";
import { existsSync, runCommand, runCommandWithOutput } from "./helpers.ts";

export const rootDir = path.resolve(
  path.join(path.fromFileUrl(import.meta.url), "../../../../"),
);

export class Crate {
  #manifestPath: string;
  #isUpdatingManifest = false;

  constructor(
    public readonly name: string,
    public readonly folderPath: string,
    public readonly repo: Repo,
  ) {
    if (!existsSync(folderPath)) {
      throw new Error(`Could not find crate ${name} at ${folderPath}`);
    }

    this.#manifestPath = path.join(this.folderPath, "Cargo.toml");
  }

  get version() {
    if (this.#isUpdatingManifest) {
      throw new Error("Cannot get version while updating manifest.");
    }
    const fileText = Deno.readTextFileSync(this.#manifestPath);
    const result = /^version = "([0-9]+\.[0-9]+\.[0-9]+)"$/m.exec(fileText);
    if (!result) {
      throw new Error(`Could not find version.`);
    }
    return semver.parse(result[1])!;
  }

  setVersion(version: semver.SemVer) {
    return this.#updateManifestFile((fileText) => {
      return fileText.replace(
        /^version = "([0-9]+\.[0-9]+\.[0-9]+)"$/m,
        `version = "${version.toString()}"`,
      );
    });
  }

  toLocalSource(crate: Crate) {
    return this.#updateManifestFile((fileText) => {
      const relativePath = path.relative(this.folderPath, crate.folderPath)
        .replace(/\\/g, "/");
      // try to replace if it had a property in the object
      const versionPropRegex = new RegExp(
        `^(${crate.name}\\b\\s.*)version\\s*=\\s*"[^"]+"`,
        "m",
      );
      const newFileText = fileText.replace(
        versionPropRegex,
        `$1path = "${relativePath}"`,
      );
      if (newFileText !== fileText) {
        return newFileText;
      }

      // now try to find if it just had a version
      const versionStringRegex = new RegExp(
        `^(\\b${crate.name}\\b\\s.*)"([=\\^])?[0-9]+[^"]+"`,
        "m",
      );
      return fileText.replace(
        versionStringRegex,
        `$1{ path = "${relativePath}" }`,
      );
    });
  }

  revertLocalSource(crate: Crate) {
    return this.#updateManifestFile((fileText) => {
      const crateVersion = crate.version.toString();
      // try to replace if it had a property in the object
      const pathOnlyRegex = new RegExp(
        `^${crate.name} = { path = "[^"]+" }$`,
        "m",
      );
      const newFileText = fileText.replace(
        pathOnlyRegex,
        `${crate.name} = "${crateVersion}"`,
      );
      if (newFileText !== fileText) {
        return newFileText;
      }

      // now try to find if it had a path in an object
      const versionStringRegex = new RegExp(
        `^(${crate.name}\\b\\s.*)path\\s*=\\s*"[^"]+"`,
        "m",
      );
      return fileText.replace(
        versionStringRegex,
        `$1version = "${crateVersion}"`,
      );
    });
  }

  build() {
    return this.runCommandWithOutput(["cargo", "build", "--all-features"]);
  }

  cargoCheck() {
    return this.runCommandWithOutput(["cargo", "check"]);
  }

  test() {
    return this.runCommandWithOutput(["cargo", "test", "--all-features"]);
  }

  runCommand(cmd: string[]) {
    return runCommand({
      cwd: this.folderPath,
      cmd,
    });
  }

  runCommandWithOutput(cmd: string[]) {
    return runCommandWithOutput({
      cwd: this.folderPath,
      cmd,
    });
  }

  async #updateManifestFile(action: (fileText: string) => string) {
    if (this.#isUpdatingManifest) {
      throw new Error("Cannot update manifest while updating manifest.");
    }
    this.#isUpdatingManifest = true;
    try {
      const originalText = await Deno.readTextFile(this.#manifestPath);
      const newText = action(originalText);
      if (originalText === newText) {
        throw new Error(`The file didn't change: ${this.#manifestPath}`);
      }
      await Deno.writeTextFile(this.#manifestPath, newText);
    } finally {
      this.#isUpdatingManifest = false;
    }
  }
}

export class Repo {
  #crates: Crate[] = [];

  constructor(
    public readonly name: string,
    public readonly folderPath: string,
  ) {
  }

  async hasLocalChanges() {
    const output = await this.runCommand([
      "git",
      "status",
      "--porcelain",
      "--untracked-files=no",
    ]);
    return output.trim().length > 0;
  }

  switchMain() {
    return this.runCommand(["git", "switch", "main"]);
  }

  pullUpstreamMain() {
    return this.runCommand(["git", "pull", "upstream", "main"]);
  }

  addSelfCrate() {
    this.#crates.push(new Crate(this.name, this.folderPath, this));
  }

  addCrate(name: string, subDirPath: string) {
    this.#crates.push(
      new Crate(name, path.join(this.folderPath, subDirPath), this),
    );
  }

  getCrate(name: string) {
    const crate = this.#crates.find((c) => c.name === name);
    if (crate == null) {
      throw new Error(`Could not find crate with name: ${crate}`);
    }
    return crate;
  }

  getCrates() {
    return [...this.#crates];
  }

  resetHard() {
    return this.runCommand(["git", "reset", "--hard"]);
  }

  branch(name: string) {
    return this.runCommandWithOutput(["git", "checkout", "-b", name]);
  }

  gitAdd() {
    return this.runCommandWithOutput(["git", "add", "."]);
  }

  commit(message: string) {
    return this.runCommandWithOutput(["git", "commit", "-m", message]);
  }

  push() {
    return this.runCommandWithOutput(["git", "push"]);
  }

  runCommand(cmd: string[]) {
    return runCommand({
      cwd: this.folderPath,
      cmd,
    });
  }

  runCommandWithOutput(cmd: string[]) {
    return runCommandWithOutput({
      cwd: this.folderPath,
      cmd,
    });
  }
}

export class Repos {
  #repos: readonly Repo[];

  constructor() {
    const esZip = createRepo("eszip");
    esZip.addCrate("eszip", ".");
    esZip.addCrate("eszip_wasm", "lib");
    const deno = createRepo("deno");
    deno.addCrate("deno_core", "core");
    deno.addCrate("deno", "cli");

    this.#repos = [
      createRepoWithSelfCrate("deno_ast"),
      createRepoWithSelfCrate("deno_graph"),
      createRepoWithSelfCrate("deno_doc"),
      esZip,
      createRepoWithSelfCrate("deno_lint"),
      createRepoWithSelfCrate("dprint-plugin-typescript"),
      deno,
    ];

    function createRepo(name: string) {
      return new Repo(name, path.join(rootDir, name));
    }

    function createRepoWithSelfCrate(name: string) {
      const repo = createRepo(name);
      repo.addSelfCrate(); // this repo has the crate as the root
      return repo;
    }
  }

  getRepos() {
    return [...this.#repos];
  }

  getCrates() {
    const crates = [];
    for (const repo of this.#repos) {
      crates.push(...repo.getCrates());
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
      for (const crate of repo.getCrates()) {
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
      const [workingCrate, otherCrate] of this.#getLocalSourceRelationships()
    ) {
      await workingCrate.revertLocalSource(otherCrate);
    }
  }

  #getLocalSourceRelationships() {
    const deno_ast = this.getCrate("deno_ast");
    const deno_graph = this.getCrate("deno_graph");
    const deno_doc = this.getCrate("deno_doc");
    const deno_lint = this.getCrate("deno_lint");
    const dprint_plugin_typescript = this.getCrate("dprint-plugin-typescript");
    const deno_cli = this.getCrate("deno");
    const deno_core = this.getCrate("deno_core");
    const eszip = this.getCrate("eszip");
    const eszipWasm = this.getCrate("eszip_wasm");

    return [
      [deno_graph, deno_ast],
      [deno_doc, deno_ast],
      [deno_doc, deno_graph],
      [eszip, deno_ast],
      [eszip, deno_graph],
      [eszipWasm, deno_graph],
      [deno_lint, deno_ast],
      [dprint_plugin_typescript, deno_ast],
      [deno_core, deno_ast],
      [deno_cli, deno_ast],
      [deno_cli, deno_graph],
      [deno_cli, deno_doc],
      [deno_cli, deno_lint],
      [deno_cli, eszip],
      [deno_cli, dprint_plugin_typescript],
    ] as [Crate, Crate][];
  }
}
