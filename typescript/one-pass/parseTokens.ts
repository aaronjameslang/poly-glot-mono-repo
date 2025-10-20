import { TreeNode } from "./src/TreeNode";

export function parseTokens(
  tokens: string[],
  index: number
): [TreeNode, number] {
  if (tokens[index] === "(") {
    const [a, i] = parseTokens(tokens, index + 1);
    if (tokens[i] !== ")") throw new Error("Expected ) at " + i);
    if (i + 1 === tokens.length) return [a, i + 1];
    const boolOp = tokens[i + 1];
    if (boolOp !== "&&" && boolOp !== "||")
      throw new Error("Expected && or || at " + (i + 1));
    const [b, j] = parseTokens(tokens, i + 2);
    if (j !== tokens.length) throw new Error("Expected end of input at " + j);
    return [{ op: boolOp, left: a, right: b }, j];
  }

  const left = tokens[index];
  const value = tokens[index + 1];
  const right = tokens[index + 2];
  const a = { left, value, right };

  const t3 = tokens[index + 3];

  if (t3 !== "&&" && t3 !== "||") return [a, index + 3];

  const boolOp = tokens[index + 3];
  const [b, i] = parseTokens(tokens, index + 4);
  return [{ op: boolOp, left: a, right: b }, i];
}
