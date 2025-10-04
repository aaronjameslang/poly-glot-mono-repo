exports.parseUnary = function parseUnary(tokens) {
  if (typeof tokens === "string") return tokens;
  const output = [];

  let i = 0;
  while (i < tokens.length) {
    const t = tokens[i];
    if (typeof t === "object") {
      output.push(parseUnary(t));
      i += 1;
      continue;
    }
    if (t !== "-") {
      output.push(t);
      i += 1;
      continue;
    }
    const prev = tokens[i - 1];
    if (!prev || "-+*/".includes(prev)) {
      const next = tokens[i + 1];
      output.push(["0", "-", parseUnary(next)]);
      i += 2;
      continue;
    }
    output.push(t);
    i += 1;
  }
  return output.length === 1 ? output[0] : output;
};
