import { fileURLToPath } from "url";
import { resolve } from "path";

// Importing CommonJS modules in ES Modules works, even if they do use require
import { double } from "./double.cjs"; // file extension is required

export const quadruple = (x) => double(double(x));

// Basic test
if (quadruple(2) !== 8) throw new Error("quadruple(2) !== 8");

// For ES Modules, solutions using require.main, module.parent and
// __dirname/__filename won’t work because they aren’t available in ESM.
// So we're stuck with this mess:
function isCli() {
  const pathToThisFile = resolve(fileURLToPath(import.meta.url));
  const pathPassedToNode = resolve(process.argv[1]);
  return pathToThisFile.includes(pathPassedToNode);
}
// Provide a basic CLI
// Usage: node quadruple.mjs 3
if (isCli()) {
  const arg = process.argv[2];
  const i = parseFloat(arg, 10);
  const o = quadruple(i);
  const msg = `quadruple(${i}) => ${o}`;
  console.log(msg);
}
