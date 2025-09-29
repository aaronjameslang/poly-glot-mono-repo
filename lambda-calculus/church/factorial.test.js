import * as LC from "@codewars/lambda-calculus";
import { readLc } from "../readLc";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = readLc(
  "church/numbers",
  "church/isZero",
  "church/dec",
  "church/factorial",
);
const { factorial } = LC.compile(source);

describe("factorial", () => {
  test.each([
    [0, 1],
    [1, 1],
    [2, 2],
    [3, 6],
    [4, 24],
  ])("factorial(%d) is %d", (input, expected) => {
    const actual = factorial(input) + 0;
    expect(actual).toBe(expected);
  });
});
