export class Tape {
  cells = new Uint8Array();
  pointer = 0;
}

/**
 * @this {Tape}
 * @param {number|number[]|string} apendix
 */
function load(apendix) {
  if (typeof apendix === "string") {
    apendix = Uint8Array.from(apendix, (c) => c.charCodeAt(0));
  }
  if (typeof apendix === "number") {
    apendix = Uint8Array.from([apendix]);
  }
  const newCells = new Uint8Array(this.cells.length + apendix.length);
  newCells.set(this.cells);
  newCells.set(apendix, this.cells.length);
  this.cells = newCells;
}

/**
 * @this {Tape}
 * @param {number=} [index]
 * @returns {number}
 */
function getByte(index) {
  const i =
    index == undefined
      ? this.pointer
      : index >= 0
      ? index
      : this.cells.length + index;
  return this.cells[i];
}

/**
 * @this {Tape}
 * @returns {number[]}
 */
function getBytes() {
  return Array.from(this.cells);
}

function setByte(value) {
  if (this.pointer >= this.cells.length) {
    const newCells = new Uint8Array(this.pointer + 1);
    newCells.set(this.cells);
    this.cells = newCells;
  }
  this.cells[this.pointer] = value;
  // console.log(`Set byte @${this.pointer} = ${value}`);
  // if (this.cells[this.pointer] != value) throw new Error("Failed to set byte");
  // console.log(`@${this.pointer} = ${this.cells[this.pointer]}`);
  // console.log(this.cells, this.cells.length);
}

/**
 * @this {Tape}
 * @param {number=} [index]
 * @returns {string}
 */
function getChar(index) {
  const i =
    index == undefined
      ? this.pointer
      : index >= 0
      ? index
      : this.cells.length + index;
  return String.fromCharCode(this.cells[i]);
}

/**
 * @this {Tape}
 * @returns {string}
 */
function getChars() {
  return String.fromCharCode(...this.cells);
}

/**
 * @this {Tape}
 * @returns {number}
 */
function read() {
  const value = this.cells[this.pointer];
  this.pointer += 1;
  return value;
}

/**
 * @this {Tape}
 */
function advance() {
  this.pointer += 1;
}

/**
 * @this {Tape}
 */
function retreat() {
  this.pointer -= 1;
}

export class InputTape extends Tape {
  load = load;
  read = read;
  getByte = getByte;
  getBytes = getBytes;
  getChar = getChar;
  getChars = getChars;
}

export class OutputTape extends Tape {
  getByte = getByte;
  getBytes = getBytes;
  getChar = getChar;
  getChars = getChars;
  write(value) {
    setByte.call(this, value);
    this.pointer += 1;
  }
}

export class MemoryTape extends Tape {
  getByte = getByte;
  getBytes = getBytes;
  getChar = getChar;
  getChars = getChars;
  advance = advance;
  retreat = retreat;
  increment() {
    this.setByte((this.getByte() ?? 0) + 1);
  }
  decrement() {
    this.setByte((this.getByte() ?? 0) - 1);
  }
  setByte = setByte;
}

export class InstructionTape extends Tape {
  advance = advance;
  load = load;
  getByte = getByte;
  getBytes = getBytes;
  getChar = getChar;
  getChars = getChars;
  seekForward() {
    if (this.getChar() !== "[") throw new Error("Expected '[' for seekForward");
    let counter = 1;
    while (counter !== 0) {
      this.pointer += 1;
      const cell = this.getChar();
      if (cell === "[") counter += 1;
      if (cell === "]") counter -= 1;
    }
    if (this.getChar() !== "]") throw new Error("Expected ']' for seekForward");
  }
  seekBackward() {
    if (this.getChar() !== "]")
      throw new Error("Expected ']' for seekBackward");
    let counter = -1;
    while (counter !== 0) {
      this.pointer -= 1;
      const cell = this.getChar();
      if (cell === "[") counter += 1;
      if (cell === "]") counter -= 1;
    }
    if (this.getChar() !== "[")
      throw new Error("Expected '[' for seekBackward");
  }
}

export function printCharTape(tape) {
  console.log(tape.constructor.name);
  console.log(tape.getChars());
  console.log(new Array(tape.pointer).fill(" ").join("") + "^");
}

export function printByteTape(tape) {
  console.log(tape.constructor.name);
  console.log(
    tape
      .getBytes()
      .map((n) => String(n).padStart(3))
      .join("|")
  );
  console.log(new Array(tape.pointer * 4 + 2).fill(" ").join("") + "^");
}
