const { findMatchingParen } = require("./findMatchingParen");

/**
 * @param {string[]} tokens
 * @returns {object} Tree of expressions, without parens
 */
exports.parseParens = function parseParens(tokens) {
  const i = tokens.indexOf("(");
  if (i === -1) return tokens;
  const j = findMatchingParen(tokens, i);
  return [
    ...tokens.slice(0, i),
    parseParens(tokens.slice(i + 1, j)),
    ...parseParens(tokens.slice(j + 1)),
  ];
};
