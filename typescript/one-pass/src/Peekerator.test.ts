import { isIterable } from "./Peekerator";
import { tokenise } from "./tokenise";

describe("isIterable", () => {
  it("array", () => {
    const actual = isIterable([]);
    expect(actual).toEqual(true);
  });
});
