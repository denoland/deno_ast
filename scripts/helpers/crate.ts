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

  async hasLocalChanges() {
    const output = await this.#runCommand([
      "git",
      "status",
      "--porcelain",
      "--untracked-files=no",
    ]);
    return output.trim().length > 0;
  }

  switchMain() {
    return this.#runCommand(["git", "switch", "main"]);
  }

  pullUpstreamMain() {
    return this.#runCommand(["git", "pull", "upstream", "main"]);
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

  resetHard() {
    return this.#runCommand(["git", "reset", "--hard"]);
  }

  branch(name: string) {
    return this.#runCommandWithOutput(["git", "checkout", "-b", name]);
  }

  gitAdd() {
    return this.#runCommandWithOutput(["git", "add", "."]);
  }

  commit(message: string) {
    return this.#runCommandWithOutput(["git", "commit", "-m", message]);
  }

  push() {
    return this.#runCommandWithOutput(["git", "push"]);
  }

  build() {
    return this.#runCommandWithOutput(["cargo", "build", "--all-features"]);
  }

  cargoCheck() {
    return this.#runCommandWithOutput(["cargo", "check"]);
  }

  test() {
    return this.#runCommandWithOutput(["cargo", "test", "--all-features"]);
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

  #runCommand(cmd: string[]) {
    return runCommand({
      cwd: this.folderPath,
      cmd,
    });
  }

  #runCommandWithOutput(cmd: string[]) {
    return runCommandWithOutput({
      cwd: this.folderPath,
      cmd,
    });
  }
}

export class Crates {
  crates: readonly Crate[];

  constructor() {
    this.crates = [
      // list in build order
      createCrate("deno_ast"),
      createCrate("deno_graph"),
      createCrate("deno_doc"),
      createCrate("eszip"),
      createCrate("deno_lint"),
      createCrate("dprint-plugin-typescript"),
      new Crate("deno", path.join(rootDir, "deno", "cli")),
    ];

    function createCrate(name: string) {
      return new Crate(name, path.join(rootDir, name));
    }
  }

  nonDenoAstCrates() {
    return this.crates.filter((c) => c.name !== "deno_ast");
  }

  get(name: string) {
    const crate = this.crates.find((c) => c.name === name);
    if (crate == null) {
      throw new Error(`Could not find crate with name ${name}.`);
    }
    return crate;
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
    const deno_ast = this.get("deno_ast");
    const deno_graph = this.get("deno_graph");
    const deno_doc = this.get("deno_doc");
    const deno_lint = this.get("deno_lint");
    const dprint_plugin_typescript = this.get("dprint-plugin-typescript");
    const deno = this.get("deno");
    const eszip = this.get("eszip");

    return [
      [deno_graph, deno_ast],
      [deno_doc, deno_ast],
      [deno_doc, deno_graph],
      [eszip, deno_ast],
      [eszip, deno_graph],
      [deno_lint, deno_ast],
      [dprint_plugin_typescript, deno_ast],
      [deno, deno_ast],
      [deno, deno_graph],
      [deno, deno_doc],
      [deno, deno_lint],
      [deno, eszip],
      [deno, dprint_plugin_typescript],
    ] as [Crate, Crate][];
  }
}
