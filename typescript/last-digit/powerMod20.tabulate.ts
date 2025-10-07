import { powerMod20 } from "./powerMod20";

const expecations = Array.from({ length: 400 }, buildTestCase);

function buildTestCase(_: never, i: number) {
  const a = Math.floor(i / 20);
  const b = i % 20;
  const exp = Number(BigInt(a) ** BigInt(b) % 20n);
  const act = powerMod20(a, b);
  const status = exp === act;

  return { a, b, exp, act, status };
}

console.table(expecations);
