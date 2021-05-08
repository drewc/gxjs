const acorn = require("acorn")
const walk = require("acorn-walk")
const { generate } = require('astring')

const defaultParseOptions = {ecmaVersion: 2020 };
function parse (input, options = {}) {
  const comments = [];
  const popts = {...defaultParseOptions, onComment: comments, ...options}
  const AST = acorn.parse(input, popts);

  AST.$comments = comments;
  // console.warn('Comments:', comments);
  return AST;

};

function ensureAST(source, options = {ecmaVersion: 2020}) {
  return (typeof source === 'string') ? parse(source) : source;
}

module.exports = ensureAST
module.exports.ensureAST = ensureAST;
module.exports.parse = parse;
module.exports.walk = walk;
module.exports.generate = generate;
