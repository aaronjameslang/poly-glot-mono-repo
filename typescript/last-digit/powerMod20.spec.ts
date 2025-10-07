import { powerMod20 } from "./powerMod20";

describe("powerMod20 random", () => {
  const table = Array.from({ length: 50 }, () => [
    Math.floor(Math.random() * 12),
    Math.floor(Math.random() * 12),
  ]);

  test.each(table)(`powerMod20(%i, %i)`, (a, b) => {
    const actual = powerMod20(a, b);
    const expected = Number(BigInt(a) ** BigInt(b) % 20n);
    expect(actual).toBe(expected);
  });
});
