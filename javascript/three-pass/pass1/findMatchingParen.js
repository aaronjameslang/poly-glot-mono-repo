/**
 * @param {string[]} tokens
 * @param {number} i
 */
exports.findMatchingParen = function findMatchingParen(tokens, i) {
  let c = 1;
  while (c > 0) {
    i += 1;
    if (i >= tokens.length) throw new Error("Mismatched parens");
    const t = tokens[i];
    if (t === "(") c += 1;
    if (t === ")") c -= 1;
  }
  return i;
};
