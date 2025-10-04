exports.pass3 = function pass3(ast) {
  if (ast.op === "imm") return ["IM " + ast.n];
  if (ast.op === "arg") return ["AR " + ast.n];

  return [
    ...pass3(ast.a),
    "PU",
    ...pass3(ast.b),
    "SW",
    "PO",
    { "+": "AD", "-": "SU", "*": "MU", "/": "DI" }[ast.op],
  ];
};
