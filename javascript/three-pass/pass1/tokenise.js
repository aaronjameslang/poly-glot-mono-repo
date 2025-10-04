/**
 *
 * @param {string} program
 * @returns {string[]}
 *
 * This could be improved by streaming the input and tokens
 */
exports.tokenise = function tokenise(program) {
  const regex = /(\w+|\d+|[-+*/\(\)\[\]])/g
  return program.match(regex);
}
