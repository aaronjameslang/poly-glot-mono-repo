import { powerMod10 } from "./powerMod10.v2";

describe("powerMod10", () => {
  const table = [
    [4, 1, 4],
    [4, 2, 6],
    [9, 7, 9],
  ];

  test.each(table)(`powerMod10(%i, %i) == %i`, (a, b, expected) => {
    expect(powerMod10(a, b)).toBe(expected);
  });
});

describe("powerMod10 random", () => {
  const table = Array.from({ length: 20 }, () => [
    Math.floor(Math.random() * 12),
    Math.floor(Math.random() * 12),
  ]);

  test.each(table)(`powerMod10(%i, %i)`, (a, b) => {
    const actual = powerMod10(a, b);
    const expected = a ** b % 10;
    expect(actual).toBe(expected);
  });
});
