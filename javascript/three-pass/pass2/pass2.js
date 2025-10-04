exports.pass2 = function pass2(ast) {
  if (!ast.a) return ast; // arg or imm

  ast.a = pass2(ast.a);
  ast.b = pass2(ast.b);

  if ((ast.a.op === "imm") & (ast.b.op === "imm")) {
    const a = ast.a.n;
    const b = ast.b.n;
    let n;
    switch (ast.op) {
      case "+":
        n = a + b;
        break;
      case "-":
        n = a - b;
        break;
      case "*":
        n = a * b;
        break;
      case "/":
        n = a / b;
        break;
    }
    return { op: "imm", n };
  }

  return ast;
};
