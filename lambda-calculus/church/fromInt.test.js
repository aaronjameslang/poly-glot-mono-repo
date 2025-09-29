/**
 * This is my attempt to understand how to get numbers out of the lc compiler so
 * I can test the result is correct
 */

import * as LC from "@codewars/lambda-calculus";
import { unchurchN } from "./unchurch";

LC.configure({
  numEncoding: "Church",
});

const { id } = LC.compile("id = \\x.x");

const resultF = `function result(arg) {
      let argEnv;
      if (arg?.term && arg?.env)
        ({ term: arg, env: argEnv } = arg);
      const termVal = new Tuple(typeof arg === "number" ? fromInt(arg) : arg, new Env(argEnv));
      if (term.name === "_")
        return runEval(new Tuple(term.body, new Env(env)));
      else
        return runEval(new Tuple(term.body, new Env(env).setThunk(term.name, termVal)));
    }`;

function describeFromInt(num, lc) {
  describe(`fromInt(${num})`, () => {
    test(`${num}|fromInt is a function`, () => {
      const fn = LC.fromInt(num);
      expect(typeof fn).toBe("object");
      // with keys
    });
    test(`${num}|fromInt|toString is correct`, () => {
      const fn = LC.fromInt(num);
      const str = fn.toString();
      expect(str).toBe(lc);
    });
    test(`${num}|fromInt|id is a function`, () => {
      const fn = id(LC.fromInt(num));
      expect(typeof fn).toBe("function");
      const str = fn.toString();
      expect(str).toBe(resultF);
    });
    test(`${num}|fromInt|id + 0 is ${num}`, () => {
      const fn = id(LC.fromInt(num));
      expect(fn + 0).toBe(num);
    });
    // TODO I don't know why this breaks for 0
    (num ? test : test.skip)(`${num}|fromInt|id|unchurch is ${num}`, () => {
      const fn = unchurchN(id(LC.fromInt(num)));
      expect(fn).toBe(num);
    });
    test(`${lc}|compile is a function`, () => {
      const { n } = LC.compile("n = " + lc);
      expect(typeof n).toBe("function");
      const str = n.toString();
      expect(str).toBe(resultF);
    });
    test(`${lc}|compile + 0 is ${num}`, () => {
      const { n } = LC.compile("n = " + lc);
      expect(n + 0).toBe(num);
    });
    // TODO I don't know why this breaks for 0
    (num ? test : test.skip)(`${lc}|compile|unchurch is ${num}`, () => {
      const { n } = LC.compile("n = " + lc);
      const m = unchurchN(n);
      expect(m).toBe(num);
    });
  });
}

describe("fromInt", () => {
  describeFromInt(0, "\\ s z . z");
  describeFromInt(1, "\\ s z . s z");
  describeFromInt(2, "\\ s z . s (s z)");
  describeFromInt(3, "\\ s z . s (s (s z))");
});
