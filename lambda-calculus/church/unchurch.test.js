import { assert } from "chai";
import { unchurchN } from "./unchurch.js";

describe("unchurch", () => {
  it("nil", () => {
    const nil = (f) => (x) => x;
    const actual = unchurchN(nil);
    assert.equal(actual, 0);
  });
  it("one", () => {
    const one = (f) => (x) => f(x);
    const actual = unchurchN(one);
    assert.equal(actual, 1);
  });
  it("two", () => {
    const two = (f) => (x) => f(f(x));
    const actual = unchurchN(two);
    assert.equal(actual, 2);
  });
});
