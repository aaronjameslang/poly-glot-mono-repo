import * as LC from "@codewars/lambda-calculus";
import fs from "fs";
import { assert } from "chai";

// Usage: `bun test *.test.js`

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = fs.readFileSync("./church-numbers.lc", "utf8");
const { inc, add, mul, pow } = LC.compile(source);

function unchurch(x) {
  return x((n) => n + 1)(0);
}

describe("Church Numbers", () => {
  it("Increment", () => {
    assert.equal(unchurch(inc(0)), 1);
    assert.equal(unchurch(inc(1)), 2);
    assert.equal(unchurch(inc(2)), 3);
  });
  it("Addition", () => {
    assert.equal(unchurch(add(1)(2)), 3);
    assert.equal(unchurch(add(2)(4)), 6);
    assert.equal(unchurch(add(0)(3)), 3);
    assert.equal(unchurch(add(0)(0)), 0);
  });
  it("Multiplication", () => {
    assert.equal(unchurch(mul(1)(2)), 2);
    assert.equal(unchurch(mul(2)(4)), 8);
    assert.equal(unchurch(mul(0)(3)), 0);
    assert.equal(unchurch(mul(0)(0)), 0);
  });
  it.skip("Exponentiation", () => {
    assert.equal(unchurch(pow(1)(1)), 1);
    assert.equal(unchurch(pow(2)(1)), 2);
    assert.equal(unchurch(pow(1)(2)), 1);
    assert.equal(unchurch(pow(2)(4)), 16);
    assert.equal(unchurch(pow(0)(3)), 0);
    assert.equal(unchurch(pow(0)(0)), 1);
  });
});
