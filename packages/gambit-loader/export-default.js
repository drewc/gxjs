const acorn = require("acorn")

function makeExportDefaultDeclaration (expression) {
  return {
    type: 'ExportDefaultDeclaration',
    declaration: expression
  }
}

function wrapExportDefault(source) {
  const AST = (typeof source === 'string') ?  acorn.parse(content) : source;
  AST.body = [makeExportDefaultDeclaration(AST.body[0])]
  return AST;
}

module.exports = wrapExportDefault;
