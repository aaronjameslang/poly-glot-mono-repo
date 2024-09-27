/**
 * Even Or Odd
 *
 * An 8th kyu problem posed by codewars.com:
 * https://www.codewars.com/kata/53da3dbb4a5168369a0000fe
 *
 * Create a function that takes an integer as an argument and returns "Even" for
 * even numbers or "Odd" for odd numbers.
 */

// Usage: bun test ./EvenOrOdd.ts

import { assert } from "chai";

describe("Example tests", function () {
  it("evenOrOdd(1) should return 'Odd'", function () {
    assert.equal(evenOrOdd(1), "Odd");
  });
  it("evenOrOdd(2) should return 'Even'", function () {
    assert.equal(evenOrOdd(2), "Even");
  });
  it("evenOrOdd(-1) should return 'Odd'", function () {
    assert.equal(evenOrOdd(-1), "Odd");
  });
  it("evenOrOdd(-2) should return 'Even'", function () {
    assert.equal(evenOrOdd(-2), "Even");
  });
  it("evenOrOdd(0) should return 'Even'", function () {
    assert.equal(evenOrOdd(0), "Even");
  });
});

export function evenOrOdd(n: number): string {
  return n % 2 === 0 ? "Even" : "Odd";
}
