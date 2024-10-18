/**
 * Transcend Mortality
 *
 * A 1st kyu problem posed by codewars.com.
 * I'll refrain from linking to the problem to discourage those who might be
 * tempted to search this solution rather than challenge themselves.
 *
 * Let l, m, n, t be integers.
 *
 * Let the matrix M be a matrix of size m x n.
 * The value of each element is the XOR of the row and column it is in.
 *
 * Let the matrix N be the matrix M, but every element less by l.
 * If the element is less than 0, it is set to 0.
 *
 * The solution a is the sum of the elements of N, modulo t.
 */

import { assert } from "chai";

function calcM_naive(m: number, n: number): number[][] {
  const M: number[][] = [];

  for (let i = 0; i < m; i += 1) {
    M[i] = [];
    for (let j = 0; j < n; j += 1) {
      M[i][j] = i ^ j;
    }
  }

  return M;
}

describe("calcM_naive", function () {
  it("3 x 3", function () {
    const m = 3;
    const n = 3;
    const expected = [
      [0, 1, 2],
      [1, 0, 3],
      [2, 3, 0],
    ];
    assert.deepEqual(calcM_naive(m, n), expected);
  });
  it("10 x 15", function () {
    const m = 10;
    const n = 15;
    const expected = [
      [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14],
      [1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15],
      [2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12],
      [3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13],
      [4, 5, 6, 7, 0, 1, 2, 3, 12, 13, 14, 15, 8, 9, 10],
      [5, 4, 7, 6, 1, 0, 3, 2, 13, 12, 15, 14, 9, 8, 11],
      [6, 7, 4, 5, 2, 3, 0, 1, 14, 15, 12, 13, 10, 11, 8],
      [7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9],
      [8, 9, 10, 11, 12, 13, 14, 15, 0, 1, 2, 3, 4, 5, 6],
      [9, 8, 11, 10, 13, 12, 15, 14, 1, 0, 3, 2, 5, 4, 7],
    ];
    assert.deepEqual(calcM_naive(m, n), expected);
  });
});

function calcN_naive(M: number[][], l: number): number[][] {
  const N: number[][] = [];

  for (let i = 0; i < M.length; i += 1) {
    N[i] = [];
    for (let j = 0; j < M[i].length; j += 1) {
      const mij = M[i][j];
      const nij = l >= mij ? 0 : mij - l;
      N[i][j] = nij;
    }
  }

  return N;
}

function sumMatrix(mat: number[][]): number {
  return mat.reduce((acc, row) => acc + sumArray(row), 0);
}

function sumArray(arr: number[]): number {
  return arr.reduce((acc, n) => acc + n, 0);
}

/**
 * Worse than linear time complexity.
 */
function calcAnswer_naive(l: number, m: number, n: number, t: number): number {
  const M = calcM_naive(m, n);
  const N = calcN_naive(M, l);
  const a = sumMatrix(N) % t;
  return a;
}

describe("calcAnswer_naive", function () {
  function t(m: number, n: number, l: number, t: number, a: number) {
    it(`m=${m}, n=${n}, l=${l}, t=${t} => ${a}`, function () {
      assert.equal(calcAnswer_naive(l, m, n, t), a);
    });
  }
  /**
   * Slow for larger values, and likely inaccurate due to integer overflow.
   * We could mod early for accuracy, but this would still be very slow.
   */
  t(8, 5, 1, 100, 5);
  t(8, 8, 0, 100007, 224);
  t(25, 31, 0, 100007, 11925);
  t(5, 45, 3, 1000007, 4323);
  t(31, 39, 7, 2345, 1586);
  t(545, 435, 342, 1000007, 808451);
  t(1e3, 1e3, 1e3, 1e3, 376); // 16ms
  t(1e4, 1e4, 1e3, 1e3, 416); // 728ms
  //   t(2e4, 2e4, 1e3, 1e3, 128); // 2889ms
  //   t(28827050410, 35165045587, 7109602, 13719506, 5456283);
});
