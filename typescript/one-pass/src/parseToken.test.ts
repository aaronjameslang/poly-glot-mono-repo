import { parseTokens } from "./parseTokens";
import { Peekerator } from "./Peekerator";

describe("tokenise", () => {
  it("name === ada", () => {
    const tokens = new Peekerator(["name", "===", "ada"]);
    const dc = parseTokens(tokens);
    expect(dc).toEqual({
      left: "name",
      op: "===",
      right: "ada",
    });
    expect(tokens.next().done).toEqual(true);
  });
  it("name === ada && age >= 35", () => {
    const tokens = new Peekerator([
      "name",
      "===",
      "ada",
      "&&",
      "age",
      ">=",
      "35",
    ]);
    const dc = parseTokens(tokens);
    expect(dc).toEqual({
      left: {
        left: "name",
        op: "===",
        right: "ada",
      },
      op: "&&",
      right: {
        left: "age",
        op: ">=",
        right: "35",
      },
    });
    expect(tokens.next().done).toEqual(true);
  });
  it("(name === ada)", () => {
    const tokens = new Peekerator(["(", "name", "===", "ada", ")"]);
    const dc = parseTokens(tokens);
    expect(dc).toEqual({
      left: "name",
      op: "===",
      right: "ada",
    });
    expect(tokens.next().done).toEqual(true);
  });
  it("(name === ada && age >= 35) || colour!==red", () => {
    const tokens = new Peekerator([
      "(",
      "name",
      "===",
      "ada",
      "&&",
      "age",
      ">=",
      "35",
      ")",
      "||",
      "colour",
      "!==",
      "red",
    ]);
    const dc = parseTokens(tokens);
    expect(dc).toEqual({
      left: {
        left: {
          left: "name",
          op: "===",
          right: "ada",
        },
        op: "&&",
        right: {
          left: "age",
          op: ">=",
          right: "35",
        },
      },
      op: "||",
      right: {
        left: "colour",
        op: "!==",
        right: "red",
      },
    });
    expect(tokens.next().done).toEqual(true);
  });
  it("name === ada && (age >= 35 || colour!==red )", () => {
    const tokens = new Peekerator([
      "name",
      "===",
      "ada",
      "&&",
      "(",
      "age",
      ">=",
      "35",
      "||",
      "colour",
      "!==",
      "red",
      ")",
    ]);
    const dc = parseTokens(tokens);
    expect(dc).toEqual({
      left: {
        left: "name",
        op: "===",
        right: "ada",
      },
      op: "&&",
      right: {
        op: "||",
        left: {
          left: "age",
          op: ">=",
          right: "35",
        },
        right: {
          left: "colour",
          op: "!==",
          right: "red",
        },
      },
    });
    expect(tokens.next().done).toEqual(true);
  });
});
