import { fileURLToPath } from "url";
import { resolve } from "path";

// Importing CommonJS modules in ES Modules works, if they don't use require
import { multiply } from "./multiply.cjs"; // file extension is required

export const triple = multiply(3);

// Basic test
if (triple(2) !== 6) throw new Error("double(2) !== 6");

// For ES Modules, solutions using require.main, module.parent and
// __dirname/__filename won’t work because they aren’t available in ESM.
// So we're stuck with this mess:
function isCli() {
  const pathToThisFile = resolve(fileURLToPath(import.meta.url));
  const pathPassedToNode = resolve(process.argv[1]);
  return pathToThisFile.includes(pathPassedToNode);
}
// Provide a basic CLI
// Usage: node triple.cjs 3.14
if (isCli()) {
  const arg = process.argv[2];
  const i = parseFloat(arg, 10);
  const o = triple(i);
  const msg = `triple(${i}) => ${o}`;
  console.log(msg);
}
