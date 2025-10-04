const { indexOfEither } = require("./indexOfEither");

describe("indexOfEither", function () {
    it("a + b", function () {
    const input = ["a", '+', 'b',];
    const actual = indexOfEither(input, '+', '-');
    const expected = 1;
    expect(actual).toEqual(expected);
  });
  it("a + b + c", function () {
    const input = ["a", '+', 'b', '+', 'c'];
    const actual = indexOfEither(input, '+', '-');
    const expected = 3;
    expect(actual).toEqual(expected);
  });
    it("a + b - c", function () {
      const input = ["a", "+", "b", "-", "c"];
      const actual = indexOfEither(input, "+", "-");
      const expected = 3;
      expect(actual).toEqual(expected);
    });
});
