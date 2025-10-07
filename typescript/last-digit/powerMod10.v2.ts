export function powerMod10(a: number, b: number): number {
  if (b == 0) return 1;
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
  const row = matrix[a % 10];
  return row[b % row.length];
}
