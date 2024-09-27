// Error [ERR_REQUIRE_ESM]: require() of ES Module not supported.
const { modulo } = require("./modulo.mjs");

const odd = modulo(2);

// Basic test
if (odd(2)) throw new Error("odd(2)");

// Exporting by setting export object to a new object
module.exports = { odd };

// Provide a basic CLI
// Usage: node odd.cjs 3
if (require.main === module) {
  const arg = process.argv[2];
  const i = parseFloat(arg, 10);
  const o = odd(i);
  const msg = `odd(${i}) => ${o}`;
  console.log(msg);
}
