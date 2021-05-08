const acorn = require("acorn")

function makeCallExpression (exp_statement, args = []) {
  return {
    type: 'ExpressionStatement',
    expression: {
      type: 'CallExpression',
      callee: exp_statement.expression ,
      arguments: args,
      optional: false
    }
  }

}

function wrapCallExpression (source, args = []) {
  const AST = (typeof source === 'string') ?  acorn.parse(content) : source;
  AST.body = [makeCallExpression(AST.body[0], args)]
  return AST;
}

module.exports = wrapCallExpression;
