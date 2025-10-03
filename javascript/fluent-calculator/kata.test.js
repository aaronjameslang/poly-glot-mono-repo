const { assert } = require("chai");
const { FluentCalculator } = require("./kata");

describe("Fluent Calculator", function () {
  it("FluentCalculator.zero == 0", function () {
    assert.equal(FluentCalculator?.zero, 0);
  });
  it("FluentCalculator.one == 1", function () {
    assert.equal(FluentCalculator?.one, 1);
  });
  it("FluentCalculator.two == 2", function () {
    assert.equal(FluentCalculator?.two, 2);
  });

  it("FluentCalculator.two.plus.three == 5", function () {
    assert.equal(FluentCalculator?.two?.plus?.three, 5);
  });
});
