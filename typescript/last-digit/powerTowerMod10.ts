export function powerTowerMod10(ns: number[]): number {
  if (ns.length === 0) return 1;
  const acc = ns.reduceRight(powerMod20Step, PowerMod20Accumulator.One());
  return acc.mod20 % 10;
}

// a^b % 20
export function powerMod20Step(
  b: PowerMod20Accumulator,
  a: number
): PowerMod20Accumulator {
  // 0^0 = 1
  // n^0 = 1
  // 1^n = 1
  if (b.isZero) return PowerMod20Accumulator.One();
  // 0^n = 0
  if (a === 0) return PowerMod20Accumulator.Zero();
  // n^1 = n
  if (b.isOne) return PowerMod20Accumulator.Mod20(a);
  const matrix = [
    [0],
    [1],
    [16, 12, 4, 8],
    [1, 3, 9, 7],
    [16, 4],
    [5],
    [16],
    [1, 7, 9, 3],
    [16, 8, 4, 12],
    [1, 9],
    [0],
    [1, 11],
    [16, 12, 4, 8],
    [1, 13, 9, 17],
    [16, 4],
    [5, 15],
    [16],
    [1, 17, 9, 13],
    [16, 8, 4, 12],
    [1, 19],
  ];
  const row = matrix[a % 20];
  const n = row[b.mod20 % row.length];
  return PowerMod20Accumulator.Mod20(n);
}

export interface PowerMod20Accumulator {
  isOne: boolean;
  isZero: boolean;
  mod20: number;
}

export function PowerMod20Accumulator(n: number): PowerMod20Accumulator {
  return {
    isOne: n === 1,
    isZero: n === 0,
    mod20: n % 20,
  };
}

PowerMod20Accumulator.Zero = () => ({ isOne: false, isZero: true, mod20: 0 });
PowerMod20Accumulator.One = () => ({ isOne: true, isZero: false, mod20: 1 });
PowerMod20Accumulator.Mod20 = (mod20: number) => ({
  isOne: false,
  isZero: false,
  mod20,
});
