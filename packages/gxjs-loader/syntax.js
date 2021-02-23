const acorn = require("acorn")
const walk = require("acorn-walk")
const { generate } = require('astring')

function parse (input, options = {ecmaVersion: 2020}) {
  return acorn.parse(input, options)
};

function ensureAST(source, options = {ecmaVersion: 2020}) {
  return (typeof source === 'string') ? parse(source) : source;
}

module.exports = ensureAST
module.exports.ensureAST = ensureAST;
module.exports.parse = parse;
module.exports.walk = walk;
module.exports.generate = generate;
