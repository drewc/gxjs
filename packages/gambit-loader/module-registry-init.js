const acorn = require("acorn")
const walk = require("acorn-walk")
const { generate } = require('astring')

function findModInit(body) {
  return body.find(e => {
    return (
      e.type === 'ExpressionStatement'
        && e.expression.type === 'CallExpression'
        && e.expression.callee.type === 'MemberExpression'
        && e.expression.callee.object.name === 'RTS'
        && e.expression.callee.property.name === 'module_registry_init'
    )
  })
}

module.exports = function (source) {
  const AST = (typeof source === 'string') ? acorn.parse(source) : source;
  const mod_init = findModInit(AST.body);
  const mods = mod_init.expression.arguments[0].elements;
  const first_literal = mods[0].arguments[0]
  const last_literal = mods[mods.length -1].arguments[0]

  first_literal.value = last_literal.value;
  first_literal.raw = last_literal.raw;

  mods.pop();

  return AST;
}
