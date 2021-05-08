const { ensureAST, generate, walk } = require('./syntax.js')

function collectIdentNames(AST) {
  const idStrings = []
  walk.simple(AST, {
    Identifier(node) { idStrings.push(node.name) }
  })
  return [...new Set(idStrings)]
}

// > const linkInfoComments = AST.$comments.filter(c => c.value.startsWith(' Link info:'))
// > linkInfoComments.map(c => c.start);
// => [ 35, 45176, 111735 ]

function divideAST(AST) {
  let linkInfo = AST.$comments.filter(c => c.value.startsWith(' Link info:'))
  const bodies = []
  let body = []

  AST.body.map(n => {
    const nextMod = (linkInfo.length === 1) ? false : linkInfo[1]
    if (!nextMod || (n.start < nextMod.start)) {
      body.push(n)
    } else {
      bodies.push(body); body = [n] ;
      linkInfo = linkInfo.slice(1);
    }
  })

  bodies.push(body);
  return bodies;

}

function isGambitLocal(nameOrIdentifier) {
  const name = (typeof nameOrIdentifier === 'string') ?
        nameOrIdentifier : nameOrIdentifier.name;
 return /^_\w\w4[A-Za-z]?$|^_\w\w\w5[A-Za-z]?$/.test(name);
}

function countAssignments(AST, local = isGambitLocal) {
  const obj = {}
  walk.simple(AST, {
    AssignmentExpression(node) {
      const id = node.left;
      if (local(id)) {
        obj[id.name] = 1 + (obj[id.name] || 0)
      }
    }})
  return obj
}

let mutables = {}
function mutablesObject(AST) {
    const ass = countAssignments(AST)
    const ess = Object.entries(ass).filter(([_, n]) => n > 1)
    return Object.fromEntries(ess)
}

function isMutable(nameOrIdentifier, mutables = mutables) {
  const name = (typeof nameOrIdentifier === 'string') ?
        nameOrIdentifier : nameOrIdentifier.name;
  return mutables.hasOwnProperty(name);
}

function AssignmentExpression2VariableDeclarator(exp) {
 return {
   type: 'VariableDeclarator',
   id: exp.left,
   init: exp.right
 }
}

function makeVariableDeclaration(decls, kind = 'const') {
  return {
    type: 'VariableDeclaration',
    declarations: decls,
    kind
  }
}

function makeBodyWithVariableDeclarations(body, ids = [], mutables = mutables) {
  const newBody = [] ;
  let decls = []
  body.map(n => {
    if (n.type === 'ExpressionStatement'
        && n.expression.type === 'AssignmentExpression'
        && isGambitLocal(n.expression.left)
        && !isMutable(n.expression.left, mutables)) {
      decls.push(AssignmentExpression2VariableDeclarator(n.expression))
      ids.push(n.expression.left.name);
    } else {
      if (decls.length > 0) {
        newBody.push(makeVariableDeclaration(decls))
        decls = []
      }
      newBody.push(n)
    }
  })

  return newBody
}

function modify2LetBoundBody(body, constIdNames = constIdNames) {
  let modIdNames = []
  body.map(n => modIdNames.push(...collectIdentNames(n)))
  modIdNames = [...new Set(modIdNames)]
  modIdNames = modIdNames.filter(name =>
    isGambitLocal(name) && !constIdNames.includes(name))
  const letDecls = modIdNames.map(name => {
    return {
      type: 'VariableDeclarator',
      id: { type: 'Identifier', name },
      init: null
    }
  })

  const letForm = makeVariableDeclaration(letDecls, 'let')
  body.unshift(letForm);
  return body;
}

function wrapArrowBlock(body) {
  const Arrow = ensureAST('(() => { return 0 })()').body[0]
  Arrow.expression.callee.body.body = body;
  return Arrow;
}

function lexicalizeBody(body, mutables = mutables) {
  const constIdNames = []
  const newBody = makeBodyWithVariableDeclarations(body, constIdNames, mutables);
  modify2LetBoundBody(newBody, constIdNames);
  return [wrapArrowBlock(newBody)];
}

function lexicalize(source, exe = false) {
  const AST = ensureAST(source),
        mutables = mutablesObject(AST),
        modBodies = divideAST(AST),
        newAST = ensureAST('{}')

  for (const i in modBodies) {
    if (!exe || i > 0) {
      modBodies[i] = lexicalizeBody(modBodies[i], mutables)
    }
  }

  newAST.body = [].concat(...modBodies)

  return newAST;
}

module.exports = lexicalize
