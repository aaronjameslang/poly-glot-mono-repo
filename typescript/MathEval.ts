/**
 * Evaluate Mathematical Expression
 *
 * A 2nd kyu problem posed by codewars.com:
 * https://www.codewars.com/kata/52a78825cdfc2cfc87000005
 *
 * Given a mathematical expression as a string you must return the result as a number.
 *
 * Numbers may be either a whole numbers and/or decimal numbers.
 *
 * You need to support the following mathematical operators:
 *     Multiplication *
 *     Division / (as floating point division)
 *     Addition +
 *     Subtraction -
 *
 * Operators are always evaluated from left-to-right, and * and / must be evaluated before + and -.
 *
 * You need to support multiple levels of nested parentheses, ex. (2 / (2 + 3.33) * 4) - -6
 */

// Usage: bun test ./MathEval.ts

import { expect } from "chai";

const tests: [string, number][] = [
  ["1+1", 2],
  ["1 - 1", 0],
  ["1* 1", 1],
  ["1 /1", 1],
  ["-123", -123],
  ["123", 123],
  ["2 /2+3 * 4.75- -6", 21.25],
  ["12* 123", 1476],
  ["2 / (2 + 3) * 4.33 - -6", 7.732],
];

describe("calc", function () {
  tests.forEach(function (m) {
    it("should evaluate " + m, () => {
      var x = calc(m[0]);
      var y = m[1];
      expect(x).to.equal(
        y,
        'Expected: "' + m[0] + '" to be ' + y + " but got " + x
      );
    });
  });
});

function calc(expr: string): number {
  return evalExpression(tokenise(expr));
}

// ------------------------------------
// Tokenisation

type Token = number | string;

function isNumericChar(c: string): boolean {
  return /\d|\./.test(c);
}

function isSpaceChar(c: string): boolean {
  return /\s/.test(c);
}

function tokenise(input: string): Token[] {
  let tokens: Token[] = [];
  let i = 0;

  while (i < input.length) {
    const c = input[i];

    if (isSpaceChar(c)) {
      i++;
    } else if (isNumericChar(c)) {
      const [token, j] = tokeniseNumeric(input, i);
      tokens.push(token);
      i = j;
    } else {
      // It's an operator or parens
      tokens.push(c);
      i++;
    }
  }

  return tokens;
}

function tokeniseNumeric(input: string, start: number): [number, number] {
  let i = start;

  while (isNumericChar(input[i])) {
    i++;
  }

  const num = parseFloat(input.slice(start, i));

  return [num, i];
}

// ------------------------------------
// Evaluation

function evalExpression(tokens: Token[]): number {
  const [result, remainder] = evalNextExpression(tokens);
  if (remainder.length > 0)
    throw new Error(`Trailing tokens: ${JSON.stringify(remainder)}`);
  return result;
}

// Expressions are composed of terms that are added or subtracted
function evalNextExpression(tokens: Token[]): [number, Token[]] {
  let [accumulator, remainder] = evalNextTerm(tokens);

  const nextIsAddSub = () => {
    const next = remainder[0];
    return next === "+" || next === "-";
  };

  while (nextIsAddSub()) {
    const op = remainder[0];
    if (typeof op !== "string") throw new Error();
    remainder = remainder.slice(1);
    const [term, r] = evalNextTerm(remainder);
    remainder = r;
    accumulator = combineTerms(accumulator, op, term);
  }

  return [accumulator, remainder];
}

function combineTerms(x: number, op: string, y: number): number {
  switch (op) {
    case "+":
      return x + y;
    case "-":
      return x - y;
    default:
      throw new Error(`Invalid operator: ${op}`);
  }
}

// Terms are things that are added or subtracted,
// they are composed of factors that are multiplied or divided
function evalNextTerm(tokens: Token[]): [number, Token[]] {
  let [accumulator, remainder] = evalNextFactor(tokens);

  const nextIsMulDiv = () => {
    const next = remainder[0];
    return next === "*" || next === "/";
  };

  while (nextIsMulDiv()) {
    const op = remainder[0];
    if (typeof op !== "string") throw new Error();
    remainder = remainder.slice(1);
    const [factor, r] = evalNextFactor(remainder);
    remainder = r;
    accumulator = combineFactors(accumulator, op, factor);
  }

  return [accumulator, remainder];
}

function combineFactors(x: number, op: string, y: number): number {
  switch (op) {
    case "*":
      return x * y;
    case "/":
      return x / y;
    default:
      throw new Error(`Invalid operator: ${op}`);
  }
}

// Factors are things that are multiplied or divided,
// they can numbers, negative numbers, or sub-expressions in parentheses
function evalNextFactor(tokens: Token[]): [number, Token[]] {
  const token = tokens[0];

  if (typeof token === "number") {
    return [token, tokens.slice(1)];
  }

  if (token === "-") {
    const [n, remainder] = evalNextFactor(tokens.slice(1));
    return [-n, remainder];
  }

  if (token === "(") {
    const [n, remainder] = evalNextExpression(tokens.slice(1));
    if (remainder[0] !== ")") throw new Error("Unmatched parentheses");
    return [n, remainder.slice(1)];
  }

  throw new Error(`Invalid next factor: ${JSON.stringify(tokens)}`);
}
