import { buildInput } from "./lib/buildInput";
import { solve } from "./solve.constant";

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
  const max = 500;
  for (let j = 200; j < max; j += 1) {
    const length = Math.round(Math.pow(10, j / 100));
    const input = buildInput(length);
    const lenStr = length.toLocaleString();
    test(`solve 10^${j}/500 = ${lenStr} quickly`, () => {
      expect(solve(input)).toBeDefined();
    });
  }
});
