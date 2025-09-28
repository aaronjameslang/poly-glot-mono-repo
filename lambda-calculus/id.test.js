import * as LC from "@codewars/lambda-calculus";
import fs from "fs";

LC.configure({});

const source = fs.readFileSync("./id.lc", "utf8");
const { id } = LC.compile(source);

describe("id", () => {
  test("id is a function", () => {
    expect(typeof id).toBe("function");
  });
  test("id('hello') is 'hello'", () => {
    expect(id("hello")).toBe("hello");
  });
});
