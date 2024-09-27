import { fileURLToPath } from "url";
import { resolve } from "path";

import { modulo } from "./modulo.mjs"; // file extension is required

export const even = (x) => !modulo(2)(x);

// Basic test
if (even(3)) throw new Error("even(3)");

// For ES Modules, solutions using require.main, module.parent and
// __dirname/__filename won’t work because they aren’t available in ESM.
// So we're stuck with this mess:
function isCli() {
  const pathToThisFile = resolve(fileURLToPath(import.meta.url));
  const pathPassedToNode = resolve(process.argv[1]);
  return pathToThisFile.includes(pathPassedToNode);
}
// Provide a basic CLI
// Usage: node even.mjs 2
if (isCli()) {
  const arg = process.argv[2];
  const i = parseFloat(arg, 10);
  const o = even(i);
  const msg = `even(${i}) => ${o}`;
  console.log(msg);
}
