// const { tokenise } = require("../tokenise");
// const { parseParens } = require("./parseParens");

// /**
//  * @param {string[]} tokens - The source program as a string
//  * @param {string[]} params - The source program as a string
//  * @returns {Object} The AST (Abstract Syntax Tree) representation of the program
//  */
// exports.parseBody = function parseBody(tokens, params) {
//   const stack = [];

//   for (const i = 0; i < tokens.length; i += 1) {
//     const token = tokens[i];
//     const n = parseInt(token);
//     if (!isNaN(n)) {
//       stack.push({ op: "imm", n });
//       continue;
//     }
//     const p = params.indexOf(token);
//     if (p != -1) {
//       stack.push({ op: "arg", n: p });
//       continue;
//     }
//   }
// };

// // terms: a + b
// // factors: a * b
// // expressions: (??)
