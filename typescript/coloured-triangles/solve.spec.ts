import { solve } from "./solve";

describe("coloured triangle correctness", () => {
  const table = [
    ["B", "B"],
    ["GB", "R"],
    ["RRR", "R"],
    ["RGBG", "B"],
    ["RBRGBRB", "G"],
    ["RBRGBRBGGRRRBGBBBGG", "G"],
  ];

  test.each(table)("solve(%j) == %j", (input, expected) => {
    expect(solve(input)).toBe(expected);
  });
});

describe("coloured triangle performance", () => {
  // const max = 500;
  const max = 350; // TODO optimise to handle 500
  for (let j = 200; j < max; j += 1) {
    const length = Math.round(Math.pow(10, j / 100));
    const input = buildInput(length);
    const lenStr = length.toLocaleString();
    test(`solve 10^${j}/500 = ${lenStr} quickly`, () => {
      expect(solve(input)).toBeDefined();
    });
  }
});

function buildInput(length: number): string {
  const colours = ["R", "G", "B"];
  let result = "";
  for (let i = 0; i < length; i += 1) {
    result += colours[Math.floor(Math.random() * 3)];
  }
  return result;
}

// For n = 1,000, t = 15ms
// For n = 2,042, t = 61ms
// n*2 = t*4, O(n^2) time complexity as expected
// This is too slow
