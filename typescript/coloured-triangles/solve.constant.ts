/**
 * I have deliberately not made it easy to understand
 * how or why this works, to encourage fellow adventurers
 * to find their own solutions.
 */
export function solve(input: string): string {
  if (input.length === 1) return input;
  const a = Math.log(input.length - 1);
  const b = a / Math.log(3);
  const c = Math.floor(b);
  const d = Math.pow(3, c) + 1;
  const e = input.length - d;
  const f = input.slice(0, e + 1);
  const g = input.slice(-(e + 1));
  const h = solve(f);
  const i = solve(g);
  return solvePair(h, i);
}

function solvePair(a: string, b: string): string {
  if (a === b) {
    return a;
  }
  if (a === "B" && b === "G") {
    return "R";
  }
  if (a === "B" && b === "R") {
    return "G";
  }
  if (a === "G" && b === "B") {
    return "R";
  }
  if (a === "G" && b === "R") {
    return "B";
  }
  if (a === "R" && b === "B") {
    return "G";
  }
  if (a === "R" && b === "G") {
    return "B";
  }
  throw new Error("unreachable");
}
