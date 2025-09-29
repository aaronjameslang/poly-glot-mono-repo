import * as LC from "@codewars/lambda-calculus";
import { readLc } from "../readLc";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = readLc("church/church-numbers", "church/dec");
const { dec } = LC.compile(source);

describe("dec", () => {
  test.each([
    [0, 0],
    [1, 0],
    [2, 1],
    [3, 2],
  ])("dec(%d) is %d", (input, expected) => {
    const actual = dec(input) + 0;
    expect(actual).toBe(expected);
  });
});
