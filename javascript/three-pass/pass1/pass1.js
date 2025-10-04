const { tokenise } = require("./tokenise");
const { parseInfixes } = require("./parseInfixes");
const { parseParens } = require("./parseParens");
const { parseUnary } = require("./parseUnary");

/**
 * First pass of the three-pass compiler
 * @param {string} program - The source program as a string
 * @returns {Object} The AST (Abstract Syntax Tree) representation of the program
 */
exports.pass1 = function pass1(program) {
  const tokens = tokenise(program);
  const i = tokens.indexOf("]");
  const params = tokens.slice(1, i);
  const body = tokens.slice(i + 1);
  const tree1 = parseParens(body);
  const tree2 = parseUnary(tree1);
  const tree3 = parseInfixes(tree2, params);
  return tree3;
};
