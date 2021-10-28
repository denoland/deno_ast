// Copyright 2018-2021 the Deno authors. All rights reserved. MIT license.

export function publishCrate(directory: string) {
  return runCargoSubCommand({
    directory,
    args: ["publish"],
  });
}

export function build(directory: string) {
  return runCargoSubCommand({
    directory,
    args: ["build", "-vv"],
  });
}

export function check(directory: string) {
  return runCargoSubCommand({
    directory,
    args: ["check"],
  });
}

async function runCargoSubCommand(params: {
  args: string[];
  directory: string;
}) {
  const p = Deno.run({
    cwd: params.directory,
    cmd: ["cargo", ...params.args],
    stderr: "inherit",
    stdout: "inherit",
  });

  const status = await p.status();
  if (!status.success) {
    throw new Error("Failed");
  }
}
