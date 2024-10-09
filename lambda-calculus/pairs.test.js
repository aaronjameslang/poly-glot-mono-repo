import * as LC from "@codewars/lambda-calculus";
import fs from "fs";
import { assert } from "chai";

// Usage: `bun test *.test.js`

LC.configure({
  purity: "Let",
  verbosity: "Concise",
});

const source = fs.readFileSync("./pairs.lc", "utf8");
const { Pair, first, second, swap } = LC.compile(source);

describe("Pairs", () => {
  const myPair = Pair("a")("b");
  it("first", () => {
    assert.equal(first(myPair), "a");
  });
  it("second", () => {
    assert.equal(second(myPair), "b");
  });
  it("swap", () => {
    assert.equal(first(swap(myPair)), "b");
    assert.equal(second(swap(myPair)), "a");
  });
});
