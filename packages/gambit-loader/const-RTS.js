const acorn = require("acorn")

function makeConstRTS() {
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
              value: 'gambit-scheme',
              raw: "'gambit-scheme'"
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

function prependConstRTS(source) {
  const AST = (typeof source === 'string') ?  acorn.parse(content) : source;
  const constRTS = makeConstRTS();

  AST.body.unshift(constRTS);
  return AST;
}

module.exports = prependConstRTS
