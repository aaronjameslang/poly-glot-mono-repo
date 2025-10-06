export function exponentMod10(a: number, b: number): number {
  if (b == 0) return 1;
  switch (a % 10) {
    case 0:
      return 0;
    case 1:
      return 1;
    case 2:
      // 2, 4, 8, 16, 32, 64, 128, 256
      // 2, 4, 8, 6, 2, 4, 8, 6
      return [2, 4, 8, 6][(b - 1) % 4];
    case 3:
      // 3, 9, 27, 81, 243, 729
      // 3, 9, 7, 1, 3, 9
      return [3, 9, 7, 1][(b - 1) % 4];
    case 4:
      // 4, 16, 64, 256
      // 4, 6, 4, 6
      return [4, 6][(b - 1) % 2];
    case 5:
      // 5, 25, 125, 625
      // 5, 5, 5, 5
      return 5;
    case 6:
      // 6, 36, 216, 1296
      // 6, 6, 6, 6
      return 6;
    case 7:
      // 7, 49, 343, 2401
      // 7, 9, 3, 1
      return [7, 9, 3, 1][(b - 1) % 4];
    case 8:
      // 8, 64, 512, 4096
      // 8, 4, 2, 6
      return [8, 4, 2, 6][(b - 1) % 4];
    case 9:
      // 9, 81, 729, 6561
      // 9, 1, 9, 1
      return [9, 1][(b - 1) % 2];
  }
  return NaN;
}
