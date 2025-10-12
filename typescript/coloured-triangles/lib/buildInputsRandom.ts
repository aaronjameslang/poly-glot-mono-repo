import { buildInput } from "./buildInput";

export function buildInputsRandom(length: number, count: number) {
  const inputs = new Array(count).fill(0).map(() => buildInput(length));
  return inputs;
}
