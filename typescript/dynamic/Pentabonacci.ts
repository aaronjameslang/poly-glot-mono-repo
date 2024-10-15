import { assert } from "chai";

/**
 * Recusive solution, O(5^n) time complexity.
 */
function pentabonacci_r(n: number): [number, number] {
  switch (n) {
    case 0:
      return [0, 0];
    case 1:
      return [1, 1];
    case 2:
      return [1, 1];
    case 3:
      return [2, 1];
    case 4:
      return [4, 1];
    default:
      const [a, ao] = pentabonacci_r(n - 1);
      const [b] = pentabonacci_r(n - 2);
      const [c] = pentabonacci_r(n - 3);
      const [d] = pentabonacci_r(n - 4);
      const [e] = pentabonacci_r(n - 5);
      const f = a + b + c + d + e;
      const fo = f % 2 ? ao + 1 : ao;
      return [f, fo];
  }
}

/**
 * Tabulation solution, O(n) time complexity, O(n) space complexity.
 */
function pentabonacci_t(n: number): [number, number] {
  const pens = [0, 1, 1, 2, 4];
  const odds = [0, 1, 1, 1, 1];

  for (let i = 5; i <= n; i += 1) {
    pens[i] =
      pens[i - 1] + pens[i - 2] + pens[i - 3] + pens[i - 4] + pens[i - 5];
    odds[i] = pens[i] % 2 ? odds[i - 1] + 1 : odds[i - 1];
  }

  return [pens[n], odds[n]];
}

// A sliding window solution would have O(1) space complexity.

// Can handle larger values of n
function pentabonacci_big_int(n: number): [bigint, number] {
  const pens = [0n, 1n, 1n, 2n, 4n];
  const odds = [0, 1, 1, 1, 1];

  for (let i = 5; i <= n; i += 1) {
    pens[i] =
      pens[i - 1] + pens[i - 2] + pens[i - 3] + pens[i - 4] + pens[i - 5];
    odds[i] = pens[i] % 2n ? odds[i - 1] + 1 : odds[i - 1];
  }

  return [pens[n], odds[n]];
}

/**
 * A version that returns correct odd counts for big n,
 * without the need for big int.
 *
 * This is ~1000x faster, and is good for Node v8 and below.
 */
function pentabonacci_mod(n: number): [number, number] {
  const pens = [0, 1, 1, 2, 4];
  const odds = [0, 1, 1, 1, 1];

  for (let i = 5; i <= n; i += 1) {
    pens[i] =
      pens[i - 1] + pens[i - 2] + pens[i - 3] + pens[i - 4] + pens[i - 5];
    pens[i] = pens[i] % 2; // Keeps values small, but preserves oddness
    odds[i] = pens[i] % 2 ? odds[i - 1] + 1 : odds[i - 1];
  }

  return [pens[n], odds[n]];
}

// If all we care about is oddness, there is probably a clever way to do this with binary.

function testPenta(
  f: (n: number) => [number | bigint, number],
  i: number,
  o?: number
) {
  if (o != null) {
    it(`${f.name}(${i}) should be ${o}`, () => {
      assert.equal(f(i)[1], o);
    });
  } else {
    it(`${f.name}(${i}) should be a number`, () => {
      assert.equal(typeof f(i)[1], "number");
    });
  }
}

describe("Pentabonacci", () => {
  testPenta(pentabonacci_r, 5, 1);
  testPenta(pentabonacci_r, 10, 3);
  testPenta(pentabonacci_r, 15, 5);
  testPenta(pentabonacci_r, 24); // 52ms
  testPenta(pentabonacci_r, 28); // 533ms
  //   testPenta(pentabonacci_r, 32); // 8s

  testPenta(pentabonacci_t, 3, 1);
  testPenta(pentabonacci_t, 4, 1);
  testPenta(pentabonacci_t, 5, 1);
  testPenta(pentabonacci_t, 10, 3);
  testPenta(pentabonacci_t, 15, 5);
  testPenta(pentabonacci_t, 24);
  testPenta(pentabonacci_t, 28);
  testPenta(pentabonacci_t, 32);
  testPenta(pentabonacci_t, 45, 15);
  testPenta(pentabonacci_t, 64);
  // testPenta(pentabonacci_t, 68, 23); // caps at 19, possibly due to number size
  testPenta(pentabonacci_t, 128); // 0.04ms

  testPenta(pentabonacci_big_int, 3, 1);
  testPenta(pentabonacci_big_int, 4, 1);
  testPenta(pentabonacci_big_int, 5, 1);
  testPenta(pentabonacci_big_int, 10, 3);
  testPenta(pentabonacci_big_int, 15, 5);
  testPenta(pentabonacci_big_int, 24);
  testPenta(pentabonacci_big_int, 28);
  testPenta(pentabonacci_big_int, 32);
  testPenta(pentabonacci_big_int, 45, 15);
  testPenta(pentabonacci_big_int, 64);
  testPenta(pentabonacci_big_int, 68, 23); // green
  testPenta(pentabonacci_big_int, 128); // 0.04ms
  testPenta(pentabonacci_big_int, 1e3); // 0.56ms
  testPenta(pentabonacci_big_int, 1e4); // 16ms
  testPenta(pentabonacci_big_int, 1e5); // 1.4s

  testPenta(pentabonacci_mod, 3, 1);
  testPenta(pentabonacci_mod, 4, 1);
  testPenta(pentabonacci_mod, 5, 1);
  testPenta(pentabonacci_mod, 10, 3);
  testPenta(pentabonacci_mod, 15, 5);
  testPenta(pentabonacci_mod, 24);
  testPenta(pentabonacci_mod, 28);
  testPenta(pentabonacci_mod, 32);
  testPenta(pentabonacci_mod, 45, 15);
  testPenta(pentabonacci_mod, 64);
  testPenta(pentabonacci_mod, 68, 23); // green
  testPenta(pentabonacci_mod, 128); // 0.04ms
  testPenta(pentabonacci_mod, 1e3); // 0.56ms
  testPenta(pentabonacci_mod, 1e4); // 0.5ms
  testPenta(pentabonacci_mod, 1e5); // 1.1ms
});
