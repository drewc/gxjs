const { ensureAST } = require('./syntax.js')
let verbose = false;
function log (...args) { if (verbose) { console.log(...args) } };
function makeModuleExportsStatement(expression) {
  return {
    type: 'ExpressionStatement',
    expression:  {
      type: 'AssignmentExpression',
      operator: '=',
      left: {
        type: 'MemberExpression',
        object: { type: 'Identifier', name: 'module' },
        property: { type: 'Identifier', name: 'exports' },
        computed: false,
        optional: false
      },
      right: expression
    }
  }
};

function wrapModuleExports(source) {
  const AST = ensureAST(source);
  const exp = AST.body[0].expression;

  if (exp === undefined) { throw 'No expression to export in body[0]' }

  AST.body[0] = makeModuleExportsStatement(exp);
  return AST;
}

module.exports = wrapModuleExports
