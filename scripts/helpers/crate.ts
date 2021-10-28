import { path } from "./deps.ts";
import { existsSync, runCommand, runCommandWithOutput } from "./helpers.ts";

export const rootDir = path.resolve(path.join(path.fromFileUrl(import.meta.url), "../../../../"));

export class Crate {
  #manifestPath: string;
  #isUpdatingManifest = false;

  constructor(public readonly name: string, public readonly folderPath: string) {
    if (!existsSync(folderPath)) {
      throw new Error(`Could not find crate ${name} at ${folderPath}`);
    }

    this.#manifestPath = path.join(this.folderPath, "Cargo.toml");
  }

  async hasLocalChanges() {
    const output = await this.#runCommand(["git", "status", "--porcelain", "--untracked-files=no"]);
    return output.trim().length > 0;
  }

  switchMain() {
    return this.#runCommand(["git", "switch", "main"]);
  }

  pullUpstreamMain() {
    return this.#runCommand(["git", "pull", "upstream", "main"]);
  }

  localSourceDependency(crate: Crate) {
    return this.#updateManifestFile(fileText => {
      const relativePath = path.relative(this.folderPath, crate.folderPath).replace(/\\/g, "/");
      // try to replace if it had a property in the object
      const versionPropRegex = new RegExp(
        `^(${crate.name}\\b\\s.*)version\\s*=\\s*"[^"]+"`,
        "gm",
      );
      const newFileText = fileText.replace(versionPropRegex, `$1path = "${relativePath}"`);
      if (newFileText !== fileText) {
        return newFileText;
      }

      // now try to find if it just had a version
      const versionStringRegex = new RegExp(
        `^(\\b${crate.name}\\b\\s.*)"([=\\^])?[0-9]+[^"]+"`,
        "gm",
      );
      return fileText.replace(versionStringRegex, `$1{ path = "${relativePath}" }`)
    });
  }

  resetHard() {
    return this.#runCommand(["git", "reset", "--hard"]);
  }

  build() {
    return this.#runCommandWithOutput(["cargo", "build"]);
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
      createCrate("deno_lint"),
      createCrate("dprint-plugin-typescript"),
      new Crate("deno", path.join(rootDir, "deno", "cli")),
    ];

    function createCrate(name: string) {
      return new Crate(name, path.join(rootDir, name));
    }
  }

  get(name: string) {
    const crate = this.crates.find(c => c.name === name);
    if (crate == null) {
      throw new Error(`Could not find crate with name ${name}.`);
    }
    return crate;
  }
}
