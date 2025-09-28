import { assert } from "chai";
import { unchurch } from "./unchurch.js";

describe("unchurch", () => {
  it("nil", () => {
    const nil = (f) => (x) => x;
    const actual = unchurch(nil);
    assert.equal(actual, 0);
  });
  it("one", () => {
    const one = (f) => (x) => f(x);
    const actual = unchurch(one);
    assert.equal(actual, 1);
  });
  it("two", () => {
    const two = (f) => (x) => f(f(x));
    const actual = unchurch(two);
    assert.equal(actual, 2);
  });
});
