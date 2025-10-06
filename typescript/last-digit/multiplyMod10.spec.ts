import { multiplyMod10 } from "./multiplyMod10";

describe("multiplyMod10", () => {
  const table = [
    [0, 5, 0],
    [3, 7, 1],
    [4, 1, 4],
    [4, 2, 8],
    [9, 7, 3],
    [9, 9, 1],
  ];

  test.each(table)(`multiplyMod10(%i, %i) == %i`, (a, b, expected) => {
    expect(multiplyMod10(a, b)).toBe(expected);
  });
});

describe("multiplyMod10 random", () => {
  const table = Array.from({ length: 20 }, () => [
    Math.floor(Math.random() * 100),
    Math.floor(Math.random() * 100),
  ]);

  test.each(table)(`multiplyMod10(%i, %i)`, (a, b) => {
    const actual = multiplyMod10(a, b);
    const expected = (a * b) % 10;
    expect(actual).toBe(expected);
  });
});
