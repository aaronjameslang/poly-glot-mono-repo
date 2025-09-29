import * as LC from "@codewars/lambda-calculus";
import fs from "fs";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = fs.readFileSync("./church/numbers.lc", "utf8");
const { pow } = LC.compile(source);

describe("pow", () => {
  test.each([
    [0, 0, 1],
    [0, 3, 0],
    [1, 1, 1],
    [1, 2, 1],
    [2, 1, 2],
    [2, 2, 4],
    [2, 4, 16],
  ])(`%d^%d => %d`, (a, b, c) => {
    const l = pow(a)(b);
    const n = l + 0;
    expect(n).toBe(c);
  });
});
