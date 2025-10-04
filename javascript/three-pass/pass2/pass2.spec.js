const { pass1 } = require("../pass1/pass1");
const { pass2 } = require("./pass2");

describe("pass2", function () {
  it("x y z", function () {
    const input = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";
    const actual = pass2(pass1(input));
    const expected = {
      op: "/",
      a: {
        op: "-",
        a: {
          op: "+",
          a: { op: "*", a: { op: "imm", n: 6 }, b: { op: "arg", n: 0 } },
          b: { op: "*", a: { op: "imm", n: 5 }, b: { op: "arg", n: 1 } },
        },
        b: { op: "*", a: { op: "imm", n: 3 }, b: { op: "arg", n: 2 } },
      },
      b: { op: "imm", n: 8 },
    };

    expect(actual).toEqual(expected);
  });
});
