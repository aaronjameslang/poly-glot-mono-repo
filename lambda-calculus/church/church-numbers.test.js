import * as LC from "@codewars/lambda-calculus";
import fs from "fs";
import { assert } from "chai";

LC.configure({
  purity: "Let",
  numEncoding: "Church",
  verbosity: "Concise",
});

const source = fs.readFileSync("./church/church-numbers.lc", "utf8");
const { nil, one, two, inc, add, mul, pow } = LC.compile(source);

describe("Church Numbers", () => {
  it("Numbers", () => {
    assert.equal(nil, 0);
    assert.equal(one, 1);
    assert.equal(two, 2);
  });
  it("Increment", () => {
    assert.equal(inc(0), 1);
    assert.equal(inc(1), 2);
    assert.equal(inc(2), 3);
  });
  it("Addition", () => {
    assert.equal(add(1)(2), 3);
    assert.equal(add(2)(4), 6);
    assert.equal(add(0)(3), 3);
    assert.equal(add(0)(0), 0);
  });
  it("Multiplication", () => {
    assert.equal(mul(1)(2), 2);
    assert.equal(mul(2)(4), 8);
    assert.equal(mul(0)(3), 0);
    assert.equal(mul(0)(0), 0);
  });
  it.skip("Exponentiation", () => {
    assert.equal(pow(1)(1), 1);
    assert.equal(pow(2)(1), 2);
    assert.equal(pow(1)(2), 1);
    assert.equal(pow(2)(4), 16);
    assert.equal(pow(0)(3), 0);
    assert.equal(pow(0)(0), 1);
  });
});
