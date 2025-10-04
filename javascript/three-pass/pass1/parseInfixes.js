const { indexOfEither } = require("./indexOfEither");

/**
 * @param {string[]} tokens
 * @param {string[]} params
 * @returns {object} Tree of expressions, without parens
 */
exports.parseInfixes = function parseInfixes(tokens, params) {
  const j = indexOfEither(tokens, "+", "-");
  if (j !== -1) {
    const op = tokens[j];
    const a = parseInfixes(tokens.slice(0, j), params);
    const b = parseInfixes(tokens.slice(j + 1), params);
    return { op, a, b };
  }

  const i = indexOfEither(tokens, "*", "/");
  if (i !== -1) {
    const op = tokens[i];
    const a = parseInfixes(tokens.slice(0, i), params);
    const b = parseInfixes(tokens.slice(i + 1), params);
    return { op, a, b };
  }

  if (tokens.length !== 1) throw new Error();

  const token = tokens[0];

  if (typeof token === "object") return parseInfixes(token, params);

  const n = parseInt(token);
  if (!isNaN(n)) return { op: "imm", n };

  const p = params.indexOf(token);
  if (p != -1) return { op: "arg", n: p };

  throw new Error("Unrecognised token: " + tokens[0]);
};
