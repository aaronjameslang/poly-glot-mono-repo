import * as LC from "@codewars/lambda-calculus";
import fs from "fs";
import { assert } from "chai";

// Usage: `bun test *.test.js`

LC.configure({
  purity: "Let",
  verbosity: "Concise",
});

const source = fs.readFileSync("./church/booleans.lc", "utf8");
const { FALSE, TRUE, and, not, or, xor } = LC.compile(source);

function unchurch(b) {
  const x = Symbol();
  const y = Symbol();
  const z = b(x)(y);
  if (z === x) return true;
  if (z === y) return false;
  throw new Error("Invalid church boolean");
}

describe("Church Booleans", () => {
  it("not true === false", () => {
    assert.equal(unchurch(not(TRUE)), false);
  });
  it("not false === true", () => {
    assert.equal(unchurch(not(FALSE)), true);
  });

  testBinaryPredicate("and", and, (a) => (b) => a && b);
  testBinaryPredicate("or", or, (a) => (b) => a || b);
  testBinaryPredicate("xor", xor, (a) => (b) => a !== b);
});

function testBinaryPredicate(name, actual, expected, a = null, b = null) {
  if (a === null) {
    testBinaryPredicate(name, actual, expected, false, false);
    testBinaryPredicate(name, actual, expected, false, true);
    testBinaryPredicate(name, actual, expected, true, false);
    testBinaryPredicate(name, actual, expected, true, true);
    return;
  }

  const actual_ = unchurch(actual(a ? TRUE : FALSE)(b ? TRUE : FALSE));
  const expected_ = expected(a)(b);

  it(`${a} ${name} ${b} === ${expected_}`, () => {
    assert.equal(actual_, expected_);
  });
}
