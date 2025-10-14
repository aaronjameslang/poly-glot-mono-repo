const { VirtualMachine } = require("../vm/VirtualMachine.js");
const { readFileSync } = require("fs");

const programme = readFileSync(__dirname + "/mul.bf", "utf8");

const table = [
  [0, 0, 0],
  [0, 1, 0],
  [1, 0, 0],
  [1, 1, 1],
  [2, 3, 6],
  [5, 7, 35],
  [10, 15, 150],
];

it.each(table)("mul %d * %d = %d", (a, b, c) => {
  const vm = new VirtualMachine();
  vm.instructions.load(programme);
  vm.input.load([a, b]);

  vm.run();

  const output = vm.output.getBytes();
  expect(output).toEqual([c]);
});
