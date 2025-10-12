import { PrintTriangleOptions } from "./printTriangle";
import { sumMod3 } from "./sumMod3";

export function printTriangleRow(
  arr: string[],
  length: number,
  options: PrintTriangleOptions = {}
) {
  const sum = options.withSum
    ? sumMod3(arr.slice(0, length).join("")) + " "
    : "";
  const prefix = new Array(arr.length - length).fill(" ").join("");
  const str = sum + prefix + arr.slice(0, length).join(" ");
  console.log(str);
}
