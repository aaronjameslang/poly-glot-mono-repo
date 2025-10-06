import { exponentMod10 } from "./exponentMod10";

describe("exponentMod10", () => {
  const table = [
    [4, 1, 4],
    [4, 2, 6],
    [9, 7, 9],
  ];

  test.each(table)(`exponentMod10(%i, %i) == %i`, (a, b, expected) => {
    expect(exponentMod10(a, b)).toBe(expected);
  });
});

describe("exponentMod10 random", () => {
  const table = Array.from({ length: 20 }, () => [
    Math.floor(Math.random() * 12),
    Math.floor(Math.random() * 12),
  ]);

  test.each(table)(`exponentMod10(%i, %i)`, (a, b) => {
    const actual = exponentMod10(a, b);
    const expected = a ** b % 10;
    expect(actual).toBe(expected);
  });
});
