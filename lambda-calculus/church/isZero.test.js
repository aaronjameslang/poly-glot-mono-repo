import * as LC from "@codewars/lambda-calculus";
import { readLc } from "../readLc";
import { unchurchB } from "./unchurch";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = readLc("church/isZero");
const { isZero, nil, one, two } = LC.compile(source);

describe("dec", () => {
  test.each([
    [0, true],
    [1, false],
    [2, false],
  ])("isZero(%d) is %b", (input, expected) => {
    const l = isZero(input);
    const b = unchurchB(l);
    expect(b).toBe(expected);
  });
});
