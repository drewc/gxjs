const { ensureAST, generate, walk } = require('./syntax.js')

let verbose = false;
function log (...args) { if (verbose) { console.log(...args) } };

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

module.exports = function (source, options = {verbose: false}) {
  const AST = ensureAST(source);
  const mod_init = findModInit(AST.body);
  const mods = mod_init.expression.arguments[0].elements;
  const first_literal = mods[0].arguments[0]
  const last_literal = mods[mods.length -1].arguments[0]

  verbose = ((v) => v === undefined ? false : v)(options['verbose']);
  log('Changing mod_reg_init from: ', generate(mod_init))

  first_literal.value = last_literal.value;
  first_literal.raw = last_literal.raw;

  mods.pop();

  log('Changed mod_reg_init to: ', generate(mod_init))
  return AST;
}
