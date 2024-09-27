const { multiply } = require("./multiply.cjs"); // file extension is required

const double = multiply(2);

// Basic test
if (double(2) !== 4) throw new Error("double(2) !== 4");

// Exporting by setting export object to a new object
module.exports = { double };

// Provide a basic CLI
// Usage: node double.cjs 3.14
if (require.main === module) {
  const arg = process.argv[2];
  const i = parseFloat(arg, 10);
  const o = double(i);
  const msg = `double(${i}) => ${o}`;
  console.log(msg);
}
