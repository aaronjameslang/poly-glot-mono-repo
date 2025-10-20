import type { Peekerator } from "./Peekerator";
import type { TreeNode } from "./TreeNode";

export function parseTokens(tokens: Peekerator<string>): TreeNode {
  const left = tokens.next().value;

  if (left === "(") {
    const left = parseTokens(tokens);
    const close = tokens.next().value;
    if (close !== ")") throw new Error("Expected )");
    const op = tokens.next().value;
    if (!op) return left;
    const right = parseTokens(tokens);
    return { op, left, right };
  }

  const a = {
    left,
    op: tokens.next().value,
    right: tokens.next().value,
  };

  // TODO I wonder if there is a way to remove this peek?
  const pika = tokens.peek().value;

  if (pika !== "&&" && pika !== "||") return a;

  const op = tokens.next().value;
  const b = parseTokens(tokens);
  return { op, left: a, right: b };
}
