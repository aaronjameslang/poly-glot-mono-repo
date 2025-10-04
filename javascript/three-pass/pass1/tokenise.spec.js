const { tokenise } = require("./tokenise");

describe("tokenise", function () {
  it("sum", function () {
    const input = "[a b] a + b";
    const actual = tokenise(input);
    const expected = ['[', 'a', 'b', ']', 'a', '+', 'b'];
    expect(actual).toEqual(expected);
  });
});
