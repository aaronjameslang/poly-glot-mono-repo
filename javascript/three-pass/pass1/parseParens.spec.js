const {} = require("./pass1");
const { parseParens } = require("./parseParens");

describe("parseParens", function () {
  it("a + b", function () {
    const input = ["a", "+", "b"];
    const actual = parseParens(input);
    const expected = ["a", "+", "b"];
    expect(actual).toEqual(expected);
  });
  it("( a + b )", function () {
    const input = ["(", "a", "+", "b", ")"];
    const actual = parseParens(input);
    const expected = [["a", "+", "b"]];
    expect(actual).toEqual(expected);
  });
  it("( a + b ) + c", function () {
    const input = ["(", "a", "+", "b", ")", "+", "c"];
    const actual = parseParens(input);
    const expected = [["a", "+", "b"], "+", "c"];
    expect(actual).toEqual(expected);
  });
});
