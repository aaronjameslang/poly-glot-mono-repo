import { multiplyMod10 } from "./multiplyMod10";

const expecations = Array.from({ length: 100 }, buildTestCase);

function buildTestCase(_: never, i: number) {
  const a = i % 10;
  const b = Math.floor(i / 10);
  const exp = (a * b) % 10;
  const act = multiplyMod10(a, b);
  const status = exp === act;

  return { a, b, exp, act, status };
}

console.table(expecations);
