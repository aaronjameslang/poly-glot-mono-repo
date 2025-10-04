const { simulate } = require("./simulate");

describe("simulate", function () {
  it("sum", function () {
    const input = ["AR 1", "SW", "AR 0", "AD"];
    const actual = simulate(input, [5, 7]);
    expect(actual).toEqual(12);
  });
});
