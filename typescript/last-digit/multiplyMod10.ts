export function multiplyMod10(a: number, b: number): number {
  return ((a % 10) * (b % 10)) % 10;
}
