export function powerMod20(a: number, b: number): number {
  if (b == 0) return 1;
  if (b == 1) return a % 20;
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
  return row[b % row.length];
}
