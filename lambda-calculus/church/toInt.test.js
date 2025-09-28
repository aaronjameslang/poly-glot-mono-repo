import * as LC from "@codewars/lambda-calculus";

LC.configure({
  numEncoding: "Church",
  verbosity: "Verbose",
});

describe("toInt", () => {
  it("nil", () => {
    const f = (f) => (x) => x;
    const nil = LC.toInt(f);
    expect(typeof nil).toBe("object");
    const str = nil.toString();
    expect(str).toBe("<primitive>");
    // nil+0 throws
  });
});
