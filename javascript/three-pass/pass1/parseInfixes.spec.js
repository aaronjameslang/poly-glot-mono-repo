const { parseInfixes } = require("./parseInfixes");

describe("parseInfixes", function () {
  it("7", function () {
    const input = ["7"];
    const actual = parseInfixes(input);
    const expected = { op: "imm", n: 7 };
    expect(actual).toEqual(expected);
  });
  it("a", function () {
    const input = ["a"];
    const actual = parseInfixes(input, ["a"]);
    const expected = { op: "arg", n: 0 };
    expect(actual).toEqual(expected);
  });
  it("a + b", function () {
    const input = ["a", "+", "b"];
    const actual = parseInfixes(input, ["a", "b"]);
    const expected = {
      op: "+",
      a: { op: "arg", n: 0 },
      b: { op: "arg", n: 1 },
    };
    expect(actual).toEqual(expected);
  });
  it("a * b + c", function () {
    const input = ["a", "*", "b", "+", "c"];
    const actual = parseInfixes(input, ["a", "b", "c"]);
    const expected = {
      op: "+",
      a: {
        op: "*",
        a: { op: "arg", n: 0 },
        b: { op: "arg", n: 1 },
      },
      b: { op: "arg", n: 2 },
    };
    expect(actual).toEqual(expected);
  });
  it("a * (b + c)", function () {
    const input = ["a", "*", ["b", "+", "c"]];
    const actual = parseInfixes(input, ["a", "b", "c"]);
    const expected = {
      op: "*",
      a: { op: "arg", n: 0 },
      b: {
        op: "+",
        a: { op: "arg", n: 1 },
        b: { op: "arg", n: 2 },
      },
    };
    expect(actual).toEqual(expected);
  });
});
