import { buildInput } from "./buildInput";
import { printTriangle } from "./printTriangle";

export function printRandomTriangle(length: number) {
  const input = buildInput(length);
  printTriangle(input);
}
