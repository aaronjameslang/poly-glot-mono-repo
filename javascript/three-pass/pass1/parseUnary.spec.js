const {} = require("./pass1");
const { parseUnary } = require("./parseUnary");

describe("parseUnary", function () {
  it("a + b", function () {
    const input = ["a", "+", "b"];
    const actual = parseUnary(input);
    const expected = ["a", "+", "b"];
    expect(actual).toEqual(expected);
  });
  it("a - b", function () {
    const input = ["a", "-", "b"];
    const actual = parseUnary(input);
    const expected = ["a", "-", "b"];
    expect(actual).toEqual(expected);
  });
  it("- a", function () {
    const input = ["-", "a"];
    const actual = parseUnary(input);
    const expected = ["0", "-", "a"];
    expect(actual).toEqual(expected);
  });
  it("a * - b", function () {
    const input = ["a", "*", "-", "b"];
    const actual = parseUnary(input);
    const expected = ["a", "*", ["0", "-", "b"]];
    expect(actual).toEqual(expected);
  });
  it("a * - b + c", function () {
    const input = ["a", "*", "-", "b", "+", "c"];
    const actual = parseUnary(input);
    const expected = ["a", "*", ["0", "-", "b"], "+", "c"];
    expect(actual).toEqual(expected);
  });
  it("a * - (b + c)", function () {
    const input = ["a", "*", "-", ["b", "+", "c"]];
    const actual = parseUnary(input);
    const expected = ["a", "*", ["0", "-", ["b", "+", "c"]]];
    expect(actual).toEqual(expected);
  });
  it("a *  (b + - c)", function () {
    const input = ["a", "*", ["b", "+", "-", "c"]];
    const actual = parseUnary(input);
    const expected = ["a", "*", ["b", "+", ["0", "-", "c"]]];
    expect(actual).toEqual(expected);
  });
  it("a * - (b + - c)", function () {
    const input = ["a", "*", "-", ["b", "+", "-", "c"]];
    const actual = parseUnary(input);
    const expected = ["a", "*", ["0", "-", ["b", "+", ["0", "-", "c"]]]];
    expect(actual).toEqual(expected);
  });
});
