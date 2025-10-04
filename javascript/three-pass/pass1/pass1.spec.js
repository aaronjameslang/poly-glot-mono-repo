const { pass1 } = require("./pass1");
const {} = require("./pass1");
const { parseParens } = require("./parseParens");

describe("pass1", function () {
  it("a + b", function () {
    const input = "[a b] a + b";
    const actual = pass1(input);
    const expected = {
      op: "+",
      a: { op: "arg", n: 0 },
      b: { op: "arg", n: 1 },
    };
    expect(actual).toEqual(expected);
  });
  it("1 + 3 + 2*2", function () {
    const input = "[] 1 + 3 + 2*2";
    const actual = pass1(input);
    const expected = {
      op: "+",
      a: { op: "+", a: { op: "imm", n: 1 }, b: { op: "imm", n: 3 } },
      b: { op: "*", a: { op: "imm", n: 2 }, b: { op: "imm", n: 2 } },
    };

    expect(actual).toEqual(expected);
  });
  it("(1 + 3 + 2*2)", function () {
    const input = "[] (1 + 3 + 2*2)";
    const actual = pass1(input);
    const expected = {
      op: "+",
      a: { op: "+", a: { op: "imm", n: 1 }, b: { op: "imm", n: 3 } },
      b: { op: "*", a: { op: "imm", n: 2 }, b: { op: "imm", n: 2 } },
    };
    expect(actual).toEqual(expected);
  });
  it("2*3*x + 5*y - 3*z", function () {
    const input = "[ x y z ] 2*3*x + 5*y - 3*z";
    const actual = pass1(input);
    const expected = {
      op: "-",
      a: {
        op: "+",
        a: {
          op: "*",
          a: { op: "*", a: { op: "imm", n: 2 }, b: { op: "imm", n: 3 } },
          b: { op: "arg", n: 0 },
        },
        b: { op: "*", a: { op: "imm", n: 5 }, b: { op: "arg", n: 1 } },
      },
      b: { op: "*", a: { op: "imm", n: 3 }, b: { op: "arg", n: 2 } },
    };
    expect(actual).toEqual(expected);
  });
  it("x y z", function () {
    const input = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";
    const actual = pass1(input);
    const expected = {
      op: "/",
      a: {
        op: "-",
        a: {
          op: "+",
          a: {
            op: "*",
            a: { op: "*", a: { op: "imm", n: 2 }, b: { op: "imm", n: 3 } },
            b: { op: "arg", n: 0 },
          },
          b: { op: "*", a: { op: "imm", n: 5 }, b: { op: "arg", n: 1 } },
        },
        b: { op: "*", a: { op: "imm", n: 3 }, b: { op: "arg", n: 2 } },
      },
      b: {
        op: "+",
        a: { op: "+", a: { op: "imm", n: 1 }, b: { op: "imm", n: 3 } },
        b: { op: "*", a: { op: "imm", n: 2 }, b: { op: "imm", n: 2 } },
      },
    };
    expect(actual).toEqual(expected);
  });
});
