/**
 * A classic example of dynamic programming, the Fibonacci sequence.
 *
 * By running `bun test ./Fibonacci.ts` you can see the performance difference between the three implementations.
 */

import { assert } from "chai";

/**
 * This naive implementation has exponential time complexity, O(2^n).
 *
 * This is because each invocation invokes itself twice.
 */
function fib_recursion(n: number): number {
  switch (n) {
    case 0:
      return 0;
    case 1:
      return 1;
    default:
      return fib_recursion(n - 1) + fib_recursion(n - 2);
  }
}

/**
 * This implementation has linear time complexity, O(n).
 *
 * But, it has a space complexity of O(n) as well.
 *
 * Besides simply requiring more memory, this means it cannot be completely in
 * primary memory for large values of n. It will have to be swapped to RAM,
 * which will be slower.
 */
function fib_tabulation(n: number): number {
  const table = [0, 1];

  for (let i = 2; i <= n; i += 1) {
    table[i] = table[i - 1] + table[i - 2];
  }

  return table[n];
}

/**
 * This implementation has linear time complexity O(n) and constant space complexity O(1).
 *
 * This is the faster of the three implementations.
 */
function fib_sliding_window(n: number): number {
  const window = [0, 1, 1];

  for (let i = 2; i <= n; i += 1) {
    window[i % 3] = window[(i - 1) % 3] + window[(i - 2) % 3];
  }

  return window[n % 3];
}

/**
 * Same as above but can handle negative numbers.
 */
function fib_sliding_window_neg(n: number): number {
  const x = fib_sliding_window(Math.abs(n));
  if (n < 0 && !(n % 2)) return -x;
  else return x;
}

/**
 * Same as sliding window above, linear time, constant space, but avoids the heap.
 *
 * By not using an array, all of our values are stored in the stack, which is faster.
 *
 * This is more than 3x faster than the array-based sliding window implementation.
 */
function fib_sliding_window_stack(n: number): number {
  let a = 0;
  let b = 1;
  let c = 1;
  let i = 2;

  while (i < n) {
    a = b + c;
    b = c + a;
    c = a + b;
    i += 3;
  }

  switch (n % 3) {
    case 0:
      return a;
    case 1:
      return b;
    case 2:
      return c;
  }
  throw new Error("unreachable");
}

/**
 * ~100x slower than the 'little int' tabulation.
 *
 * Eats a butt load of memory, worse than linear.
 */
function fib_tab_big_int(n: number): bigint {
  const table = [0n, 1n, 1n];

  for (let i = 2; i <= n; i += 1) {
    table[i] = table[i - 1] + table[i - 2];
  }

  return table[n];
}

/**
 * Faster than above, still slower than 'little int' obv
 */
function fib_sw_big_int(n: number): bigint {
  const window = [0n, 1n, 1n];

  for (let i = 2; i <= n; i += 1) {
    window[i % 3] = window[(i - 1) % 3] + window[(i - 2) % 3];
  }

  return window[n % 3];
}

/**
 * Only marginally faster than the array above, which makes sense as bigints go
 * on the heap so this isn't really stack-based.
 */
function fib_sw_st_big_int(n: number): bigint {
  let a = 0n;
  let b = 1n;
  let c = 1n;
  let i = 2;

  while (i < n) {
    a = b + c;
    b = c + a;
    c = a + b;
    i += 3;
  }

  switch (n % 3) {
    case 0:
      return a;
    case 1:
      return b;
    case 2:
      return c;
  }
  throw new Error("unreachable");
}

/**
 * Recusrion, but linear time and constant space.
 * This function only calls itself once.
 * Performance is comparable to the tabulation and sliding window implementations.
 * Slower than the sliding window stack implementation.
 */
function fib_recursion_linear(n: number, acc = 0, last = 1) {
  if (n === 0) return acc;
  return fib_recursion_linear(n - 1, acc + last, acc);
}

function testFib(
  f: (n: number) => number | bigint,
  i: number,
  o?: number | bigint
) {
  if (o != null) {
    it(`${f.name}(${i}) should be ${o}`, () => {
      assert.equal(f(i), o);
    });
  } else {
    it(`${f.name}(${i}) should be a number`, () => {
      assert.nestedInclude(["bigint", "number"], typeof f(i));
    });
  }
}

describe("Fibonacci", () => {
  testFib(fib_recursion, 12, 144);
  testFib(fib_recursion, 16, 987);
  testFib(fib_recursion, 32, 2178309); // 12ms
  //   testFib(fib_recursion, 48, 4807526976); // 15 s
  //   testFib(fib_recursion, 64, ?); // appears to hang

  testFib(fib_tabulation, 12, 144);
  testFib(fib_tabulation, 16, 987);
  testFib(fib_tabulation, 32, 2178309);
  testFib(fib_tabulation, 48, 4807526976);
  testFib(fib_tabulation, 64, 10610209857723);
  testFib(fib_tabulation, 128);
  testFib(fib_tabulation, 256);
  testFib(fib_tabulation, 1024);
  testFib(fib_tabulation, 4096); // <1 ms
  testFib(fib_tabulation, 10e3);
  testFib(fib_tabulation, 10e6);
  testFib(fib_tabulation, 10e7); // 450ms
  // testFib(fib_tabulation, 10e8); // hangs

  testFib(fib_sliding_window, 12, 144);
  testFib(fib_sliding_window, 16, 987);
  testFib(fib_sliding_window, 32, 2178309);
  testFib(fib_sliding_window, 48, 4807526976);
  testFib(fib_sliding_window, 64, 10610209857723);
  testFib(fib_sliding_window, 128);
  testFib(fib_sliding_window, 256);
  testFib(fib_sliding_window, 1024);
  testFib(fib_sliding_window, 4096); // <1 ms
  testFib(fib_sliding_window, 10e3);
  testFib(fib_sliding_window, 10e6);
  testFib(fib_sliding_window, 10e7); // 223ms
  // testFib(fib_sliding_window, 10e8); // 2.3s

  testFib(fib_sliding_window_neg, 0, 0);
  testFib(fib_sliding_window_neg, -1, 1);
  testFib(fib_sliding_window_neg, -2, -1);
  testFib(fib_sliding_window_neg, -3, 2);
  testFib(fib_sliding_window_neg, -6, -8);
  testFib(fib_sliding_window_neg, 12, 144);
  testFib(fib_sliding_window_neg, 16, 987);
  testFib(fib_sliding_window_neg, 10e7); // 300ms
  // testFib(fib_sliding_window_neg, 10e8); // 2.9s

  testFib(fib_sliding_window_stack, 0, 0);
  testFib(fib_sliding_window_stack, 1, 1);
  testFib(fib_sliding_window_stack, 2, 1);
  testFib(fib_sliding_window_stack, 3, 2);
  testFib(fib_sliding_window_stack, 4, 3);
  testFib(fib_sliding_window_stack, 5, 5);
  testFib(fib_sliding_window_stack, 6, 8);
  testFib(fib_sliding_window_stack, 12, 144);
  testFib(fib_sliding_window_stack, 16, 987);
  testFib(fib_sliding_window_stack, 10e7); // 76ms
  testFib(fib_sliding_window_stack, 10e8); // 750ms
  // testFib(fib_sliding_window_stack, 10e9); // 6.8s

  testFib(fib_tab_big_int, 12, 144);
  testFib(fib_tab_big_int, 16, 987);
  testFib(fib_tab_big_int, 50, 12586269025);
  testFib(fib_tab_big_int, 75, 2111485077978050); // pushing what a little int can handle
  testFib(fib_tab_big_int, 100, 354224848179261915075n);
  testFib(fib_tab_big_int, 10e3); // 2ms
  testFib(fib_tab_big_int, 10e4); // 167ms
  // testFib(fib_tab_big_int, 10e5); // slow, eats at least 16GB of memory and starts swapping

  testFib(fib_sw_big_int, 12, 144);
  testFib(fib_sw_big_int, 16, 987);
  testFib(fib_sw_big_int, 50, 12586269025);
  testFib(fib_sw_big_int, 75, 2111485077978050); // pushing what a little int can handle
  testFib(fib_sw_big_int, 100, 354224848179261915075n);
  testFib(fib_sw_big_int, 10e3); // 2ms
  testFib(fib_sw_big_int, 10e4); // 138ms
  // testFib(fib_sw_big_int, 10e5); // 9.4s

  testFib(fib_sw_st_big_int, 12, 144);
  testFib(fib_sw_st_big_int, 16, 987);
  testFib(fib_sw_st_big_int, 50, 12586269025);
  testFib(fib_sw_st_big_int, 75, 2111485077978050); // pushing what a little int can handle
  testFib(fib_sw_st_big_int, 100, 354224848179261915075n);
  testFib(fib_sw_st_big_int, 10e3); // 0.6ms
  testFib(fib_sw_st_big_int, 10e4); // 101ms
  // testFib(fib_sw_st_big_int, 10e5); // 9.0s

  testFib(fib_recursion_linear, 12, 144);
  testFib(fib_recursion_linear, 16, 987);
  testFib(fib_recursion_linear, 10e3); // 0.08ms
  testFib(fib_recursion_linear, 10e4); // 0.25ms
  testFib(fib_recursion_linear, 10e7); // 349ms
  // testFib(fib_recursion_linear, 10e8); // 3.4ms
});