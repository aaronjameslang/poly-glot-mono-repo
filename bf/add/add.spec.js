const { VirtualMachine } = require("../vm/VirtualMachine.js");
const { readFileSync } = require("fs");

const programme = readFileSync(__dirname + "/add.bf", "utf8");

const table = [
  [1, 2, 3],
  [5, 7, 12],
  [10, 15, 25],
];

it.each(table)("add %d + %d = %d", (a, b, c) => {
  const vm = new VirtualMachine();
  vm.instructions.load(programme);
  vm.input.load([a, b]);

  vm.run();

  const output = vm.output.getBytes();
  expect(output).toEqual([c]);
});
