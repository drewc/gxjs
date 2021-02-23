const { ensureAST, generate, walk } = require('./syntax.js')


let verbose = false;
function log (...args) { if (verbose) { console.log(...args) } };
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

function wrapCallExpression (source, args = [], options = { verbose: false }) {
  const AST = ensureAST(source);
  AST.body = [makeCallExpression(AST.body[0], args)]
  verbose = ((v) => v === undefined ? false : v)(options['verbose']);
  log('Calling', AST.body[0].type, 'with', args)
  return AST;
}

module.exports = wrapCallExpression;
