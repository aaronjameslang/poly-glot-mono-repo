const { VirtualMachine } = require("../vm/VirtualMachine.js");

it("echoes X", () => {
  const vm = new VirtualMachine();
  vm.instructions.load(",.");
  vm.input.load("X");

  vm.run();

  const output = vm.output.getChars();
  console.log(vm.output.cells.toString());
  expect(output).toBe("X");
});

it.only("echoes Hello, World!", () => {
  const vm = new VirtualMachine();
  vm.instructions.load(",[.,]");
  vm.input.load("Hello, World!\0");

  vm.run();

  const output = vm.output.getChars();
  expect(output).toBe("Hello, World!");
});
