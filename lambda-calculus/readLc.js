import fs from "fs";

export function readLc(...names) {
  const paths = names.map((n) => `./${n}.lc`);
  const texts = paths.map((p) => fs.readFileSync(p, "utf8"));
  return texts.join("\n");
}
