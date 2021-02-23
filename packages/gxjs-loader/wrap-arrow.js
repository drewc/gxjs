const { ensureAST, generate, walk } = require('./syntax.js')

let verbose = false;
function log (...args) { if (verbose) { console.log(...args) } };

function makeID(name) { return { type: 'Identifier', name: name } };

function makeArrow(_body, args = false) {
  const params = (args instanceof Array) ? args.map(makeID) : [];
  const body = { type: 'BlockStatement', body: _body }

  return {
    type: 'ExpressionStatement',
    expression: {
      type: 'ArrowFunctionExpression',
      params: params,
      body: body,
      id: null,
      expression: false,
      generator: false,
      async: false,
    }
  }
};


function makeReturn(id) {
 return {
    type: 'ReturnStatement',
    argument: id
  }
}

function wrapArrow(source, args = false, returnArgument = false, options = { verbose: false }) {
  const AST = ensureAST(source);
  const arrow = makeArrow(AST.body, args)
  const arrowBody = arrow.expression.body.body;
  AST.body = [arrow]
  verbose = ((v) => v === undefined ? false : v)(options['verbose']);
  if (returnArgument) {
    if (typeof returnArgument === 'string') {
      returnArgument = makeReturn(makeID(returnArgument));
      arrowBody.push(returnArgument);
    }
  }
  log("Wrapping arrow with args?", args, 'and return?', returnArgument);
  return AST;
}

module.exports = wrapArrow;
