import { tokenise } from "./tokenise";

describe("tokenise", () => {
  it("name === bob", () => {
    const gen = tokenise("name === bob");
    const tokens = Array.from(gen);
    expect(tokens).toEqual(["name", "===", "bob"]);
  });
  it("(name === bob && age >= 18) || colour!==red", () => {
    const gen = tokenise("(name === bob && age >= 18) || colour!==red");
    const tokens = Array.from(gen);
    expect(tokens).toEqual([
      "(",
      "name",
      "===",
      "bob",
      "&&",
      "age",
      ">=",
      "18",
      ")",
      "||",
      "colour",
      "!==",
      "red",
    ]);
  });
});
