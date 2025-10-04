const { pass1 } = require("../pass1/pass1");
const { pass2 } = require("../pass2/pass2");
const { simulate } = require("../simulate");
const { pass3 } = require("./pass3");

describe("pass3", function () {
  it("x y z, [4, 0, 0]", function () {
    const input = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";
    const instructions = pass3(pass2(pass1(input)));
    const actual = simulate(instructions, [4, 0, 0]);
    expect(actual).toEqual(3);
  });
  it("x y z, [4, 8, 0]", function () {
    const input = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";
    const instructions = pass3(pass2(pass1(input)));
    const actual = simulate(instructions, [4, 8, 0]);
    expect(actual).toEqual(8);
  });
  it("x y z, [4, 8, 16]", function () {
    const input = "[ x y z ] ( 2*3*x + 5*y - 3*z ) / (1 + 3 + 2*2)";
    const instructions = pass3(pass2(pass1(input)));
    const actual = simulate(instructions, [4, 8, 16]);
    expect(actual).toEqual(2);
  });
});
