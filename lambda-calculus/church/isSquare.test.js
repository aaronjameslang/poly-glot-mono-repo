import * as LC from "@codewars/lambda-calculus";
import { readLc } from "../readLc";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = readLc(
  "id",
  "church/numbers",
  "church/Y",
  "church/dec",
  "church/booleans",
  "church/isZero",
  "church/isSquare",
);
const { isSquare, id } = LC.compile(source);

describe("dec", () => {
  test.each([
    // [0, true],
    [1, true],
    [2, false],
    [3, false],
    [4, true],
    [5, false],
    [6, false],
    [7, false],
    [8, false],
    [9, true],
  ])("isSquare(%d) is %s", (input, expected) => {
    const l = isSquare(input);
    const b = !!(l + 0);
    expect(b).toBe(expected);
  });
});
