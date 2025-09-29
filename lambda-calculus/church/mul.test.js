import * as LC from "@codewars/lambda-calculus";
import fs from "fs";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = fs.readFileSync("./church/numbers.lc", "utf8");
const { mul } = LC.compile(source);

describe("mul", () => {
  test.each([
    [0, 0, 0],
    [0, 3, 0],
    [1, 1, 1],
    [1, 2, 2],
    [2, 4, 8],
  ])(`%d * %d => %d`, (a, b, c) => {
    const l = mul(a)(b);
    const n = l + 0;
    expect(n).toBe(c);
  });
});
