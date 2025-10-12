import { stepDown } from "../solve.quadractic";
import { printTriangleRow } from "./printTriangleRow";

export interface PrintTriangleOptions {
  withSum?: boolean;
}

export function printTriangle(
  input: string,
  options: PrintTriangleOptions = {}
) {
  const arr = input.split("");
  let length = arr.length;
  while (length > 1) {
    printTriangleRow(arr, length, options);
    stepDown(arr, length);
    length -= 1;
  }

  printTriangleRow(arr, length, options);
  console.log("");
}
