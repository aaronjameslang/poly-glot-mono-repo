/**
 * Where a 'jump' from a number is defined as the number minus the sum of its
 * digits, how many jumps to reach zero?
 *
 * Usage: `bun test ./JumpToZero.ts`
 */

import { assert } from "chai";

function jump(origin: number) {
  let destination = origin;
  let digits = origin;
  while (digits > 0) {
    const digit = digits % 10;
    destination -= digit;
    digits = (digits - digit) / 10;
  }
  return destination;
}

/**
 * Recursion
 *
 * Linear time, stack overflow due to recursion.
 */
function countJumps_recursion(n: number): number {
  if (n === 0) return 0;
  const destination = jump(n);
  return countJumps_recursion(destination) + 1;
}
function countJumpss_recursion(ns: number[]): number[] {
  return ns.map(countJumps_recursion);
}

testCount(countJumps_recursion, 18, 2);
testCount(countJumps_recursion, 19, 2);
testCount(countJumps_recursion, 20, 3);

testCounts(countJumpss_recursion, 1e3, 1e4); // 10ms
testCounts(countJumpss_recursion, 1e3, 1e5); // 80ms
testCounts(countJumpss_recursion, 1e3, 1e6); // 700ms
// testCounts(countJumpss_r, 1e3, 1e7); // stack overflow

/**
 * Memoisation
 *
 * Linear time, but much faster.
 */
let memos: number[];
function countJumps_memoised(n: number): number {
  if (n === 0) return 0;
  if (memos[n]) return memos[n];
  const destination = jump(n);
  const count = countJumps_memoised(destination) + 1;
  memos[n] = count;
  return count;
}
function countJumpss_memoised(ns: number[]): number[] {
  memos = [];
  return ns.map(countJumps_memoised);
}

memos = [];
testCount(countJumps_memoised, 18, 2);
testCount(countJumps_memoised, 19, 2);
testCount(countJumps_memoised, 20, 3);

testCounts(countJumpss_memoised, 1e3, 1e5); // 1.2ms
// testCounts(countJumpss_memoised, 1e3, 1e6); // 4ms, sometimes stack overflow
// testCounts(countJumpss_memoised, 1e3, 1e7); // stack overflow

/**
 * Loop
 *
 * Looping to avoid stack overflow, no faster than recursion.
 */
function countJumps_loops(n: number): number {
  if (n === 0) return 0;
  let jumps = 0;
  while (n > 0) {
    n = jump(n);
    jumps += 1;
  }
  return jumps;
}
function countJumpss_loops(ns: number[]): number[] {
  return ns.map(countJumps_loops);
}

testCount(countJumps_loops, 18, 2);
testCount(countJumps_loops, 19, 2);
testCount(countJumps_loops, 20, 3);

testCounts(countJumpss_loops, 1e3, 1e5);
testCounts(countJumpss_loops, 1e3, 1e6); // 600ms, comparable to recursion
// testCounts(countJumpss_loops, 1e3, 1e7); // 6s, no stack overflow
// testCounts(countJumpss_loops, 1e3, 2e7); // 12s

/**
 * Loop memoised
 *
 * Comparable to loop without memo,
 * ~100x slower than recursion with memo,
 * because this is only memoising the last value, not each step
 */
function countJumps_loop_memo(n: number): number {
  if (n === 0) return 0;
  let jumps = 0;
  while (n > 0) {
    n = jump(n);
    jumps += 1;
  }
  memos[n] = jumps;
  return jumps;
}
function countJumpss_loop_memo(ns: number[]): number[] {
  memos = [];
  return ns.map(countJumps_loop_memo);
}

testCount(countJumps_loop_memo, 18, 2);
testCount(countJumps_loop_memo, 19, 2);
testCount(countJumps_loop_memo, 20, 3);

testCounts(countJumpss_loop_memo, 1e3, 1e5); // 60ms
testCounts(countJumpss_loop_memo, 1e3, 1e6); // 600ms
// testCounts(countJumpss_loop_memo, 1e3, 1e7); // 6s

/**
 * Tabulation
 *
 * Linear time, no stack overflow.
 * Comparable to memoised recursion, in speed.
 */
let table: number[] = [0];
function countJumps_table(n: number): number {
  if (table[n]) return table[n];
  for (let i = table.length; i <= n; i += 1) {
    const destination = jump(i);
    table[i] = table[destination] + 1;
  }
  return table[n];
}
function countJumpss_table(ns: number[]): number[] {
  table = [0];
  return ns.map(countJumps_table);
}
testCount(countJumps_table, 18, 2);
testCount(countJumps_table, 19, 2);
testCount(countJumps_table, 20, 3);

testCounts(countJumpss_table, 1e3, 1e4); // .3ms
testCounts(countJumpss_table, 1e3, 1e5); // 1.8ms
testCounts(countJumpss_table, 1e3, 1e6); // 18ms
testCounts(countJumpss_table, 1e3, 1e7); // 227ms
testCounts(countJumpss_table, 1e3, 1e8); // 2.3s

// ---- Test Helpers ----------------------------------------------------------

function testCount(f: (n: number) => number, i: number, o?: number) {
  if (o != null) {
    it(`${f.name}(${i}) should be ${o}`, () => {
      assert.equal(f(i), o);
    });
  } else {
    it(`${f.name}(${i}) should be a number`, () => {
      assert.equal(typeof f(i), "number");
    });
  }
}

function testCounts(f: (ns: number[]) => number[], l: number, m: number) {
  const arr = new Array(l);
  for (let i = 0; i < l; i += 1) {
    arr[i] = Math.floor(Math.random() * m);
  }
  it(`${
    f.name
  }(${l.toExponential()}*[...${m.toExponential()}]) should all be numbers`, () => {
    const output = f(arr);
    const isNumber = (n: number) => typeof n == "number";
    const allNumbers = output.every(isNumber);
    assert.isTrue(allNumbers);
  });
}
