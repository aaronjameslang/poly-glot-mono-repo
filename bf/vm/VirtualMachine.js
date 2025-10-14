import {
  InputTape,
  OutputTape,
  MemoryTape,
  InstructionTape,
  printCharTape,
  printByteTape,
} from "./Tape.js";

export class VirtualMachine {
  input = new InputTape();
  output = new OutputTape();
  memory = new MemoryTape();
  instructions = new InstructionTape();

  run() {
    const timeout = 100;
    const then = Date.now();
    while (this.status() === "ready") {
      if (Date.now() - then > timeout)
        throw new Error("Execution timeout: " + timeout + "ms");
      this.step();
    }
  }

  status() {
    if (this.instructions.pointer >= this.instructions.cells.length) {
      return "halted";
    }
    if (
      this.input.pointer >= this.input.cells.length &&
      this.instructions.getChar() === ","
    ) {
      return "waiting";
    }
    return "ready";
  }

  step() {
    // console.debug({
    //   instructionPointer: this.instructions.pointer,
    //   instructions: this.instructions.getChars(),
    //   memoryPointer: this.memory.pointer,
    //   memory: this.memory.getBytes(),
    //   inputPointer: this.input.pointer,
    //   input: this.input.getChars(),
    //   outputPointer: this.output.pointer,
    //   output: this.output.getChars(),
    // });

    // printCharTape(this.instructions);
    // printByteTape(this.memory);
    // printByteTape(this.input);
    // printByteTape(this.output);
    // console.log("-----");

    if (this.instructions.pointer < 0) {
      throw new Error("Instruction pointer out of bounds");
    }
    if (this.instructions.pointer >= this.instructions.cells.length) {
      throw new Error("Instruction pointer out of bounds");
    }

    const instruction = this.instructions.getChar();
    switch (instruction) {
      case ">":
        this.memory.advance();
        this.instructions.advance();
        break;
      case "<":
        this.memory.retreat();
        if (this.memory.pointer < 0) {
          throw new Error("Memory pointer out of bounds");
        }
        this.instructions.advance();
        break;
      case "+":
        this.memory.increment();
        this.instructions.advance();
        break;
      case "-":
        this.memory.decrement();
        this.instructions.advance();
        break;
      case ".":
        this.output.write(this.memory.getByte() ?? 0);
        this.instructions.advance();
        break;
      case ",":
        // Check if input is available
        if (this.input.pointer >= this.input.cells.length) {
          throw new Error("Input pointer out of bounds");
        }
        this.memory.setByte(this.input.read());
        this.instructions.advance();
        break;
      case "[":
        if ((this.memory.getByte() ?? 0) === 0) {
          this.instructions.seekForward();
        }
        this.instructions.advance();
        break;
      case "]":
        if ((this.memory.getByte() ?? 0) !== 0) {
          this.instructions.seekBackward();
        }
        this.instructions.advance();
        break;
      default:
        // Ignore anything else
        this.instructions.advance();
        break;
    }
  }
}
