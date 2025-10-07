import { powerTowerMod10 } from "./powerTowerMod10";

describe("powerTowerMod10", () => {
  const table: [number[], number][] = [
    [[], 1],
    [[0, 0], 1],
    [[1, 2], 1],
    [[0, 0, 0], 0],
    [[2, 2, 1], 4],
    [[3, 4, 5], 1],
    [[4, 3, 6], 4],
    [[7, 6, 1], 9],
    [[7, 6, 21], 1],
    [[12, 30, 21], 6],
    [[2, 2, 1, 2], 4],
    [[2, 2, 2, 0], 4],
    [[2, 2, 1, 1, 2], 4],
    [[3, 2, 1, 4, 4], 9],
    [[937640, 767456, 981242], 0],
    [[123232, 694022, 140249], 6],
    [[499942, 898102, 846073], 6],
  ];

  test.each(table)(`powerTowerMod10(%j) == %i`, (ns, expected) => {
    expect(powerTowerMod10(ns)).toBe(expected);
  });
});
