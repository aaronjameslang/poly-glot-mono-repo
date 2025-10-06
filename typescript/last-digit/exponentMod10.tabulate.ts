import { exponentMod10 } from "./exponentMod10";

const expecations = Array.from({ length: 100 }, buildTestCase);

function buildTestCase(_: never, i: number) {
  const a = Math.floor(i / 10);
  const b = i % 10;
  const exp = a ** b % 10;
  const act = exponentMod10(a, b);
  const status = exp === act;

  return { a, b, exp, act, status };
}

console.table(expecations);
