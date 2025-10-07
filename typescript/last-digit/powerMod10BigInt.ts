export function powerMod10BigInt(a: bigint, b: bigint): bigint {
  if (b == 0n) return 1n;
  const i = Number(a % 10n);
  const j = Number(b % 4n);
  const matrix = [
    [0],
    [1],
    [6, 2, 4, 8],
    [1, 3, 9, 7],
    [6, 4],
    [5],
    [6],
    [1, 7, 9, 3],
    [6, 8, 4, 2],
    [1, 9],
  ];
  const row = matrix[i];
  const result = row[j % row.length];
  return BigInt(result);
}
