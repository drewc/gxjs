const { ensureAST, generate } = require('./syntax.js')

let verbose = false;
function log (...args) { if (verbose) { console.log(...args) } };
function makeConstRTS(value = 'gerbil-scheme') {
  const raw = "'" + value + "'"
  const rts = {
    type: 'VariableDeclaration',
    declarations: [
      {
        type: 'VariableDeclarator',
        id: { type: 'Identifier', name: 'RTS' },
        init: {
          type: 'CallExpression',
          callee: { type: 'Identifier', name: 'require' },
          arguments: [
            {
              type: 'Literal',
              value: value,
              raw: raw
            }
          ],
          optional: false
        }
      }
    ],
    kind: 'const'
  };
  return rts;
}

function prependConstRTS(source, id = 'gxjs', options = { verbose: false }) {
  const AST = ensureAST(source);
  const constRTS = makeConstRTS(id);
  verbose = ((v) => v === undefined ? false : v)(options['verbose']);
  log("Adding", generate(constRTS), 'to top')
  AST.body.unshift(constRTS);
  return AST;
}

module.exports = prependConstRTS
