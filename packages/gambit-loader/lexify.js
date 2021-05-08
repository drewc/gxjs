const acorn = require("acorn")
const walk = require("acorn-walk")
const { generate } = require('astring')

function ass2var(assign) {
  var vardec = {
    type: 'VariableDeclaration',
    declarations: [
      {
        type: 'VariableDeclarator',
        id: assign.expression.left,
        init: {
          type: 'FunctionExpression',
          id: null,
          expression: false,
          generator: false,
          async: false,
          params: [],
          body: { type: 'BlockStatement', body: [] }
        }
      }
    ],
    kind: 'var'
  };
  return vardec;
}

// Variable declaration
function varDecP (obj) {return (obj.type === 'VariableDeclaration')}

// Assigment Expression
function isAssP (obj) {
  return (obj.type === 'ExpressionStatement'
          && obj.expression.type === 'AssignmentExpression'
          && obj.expression.left.type === 'Identifier'
          && obj.expression.operator === '=')
}

// Global?
function isGlobalAssP (obj, decs) {
  const name = obj.expression.left.name;
  // console.log('Global?', name,
  // decs.map(dec => dec.declarations.map(d => d.id.name === name)));
  return !decs.find(dec => dec.declarations.find(d => d.id.name === name));
}

// Make a declaration
function makeVarDecFromAss(ass) {
  return {
    type: 'VariableDeclaration',
    kind: 'let',
    declarations: [
    {
      type: 'VariableDeclarator',
      id: ass.expression.left,
      init: ass.expression.right
    }
  ]
  }
}

// Mutate the assigment
function mutateAssIntoDec (ass) {
  const dec = makeVarDecFromAss(ass);
  const keys = Object.keys(ass);
  keys.map(k => { delete ass[k] });
  Object.assign(ass, dec);
  return dec;
}

function findVarDecs(node, collection = []) {
  if (node.body === undefined) { return collection }
  if (node.body instanceof Array) {
    node.body.filter(varDecP).map (dec => collection.push(dec))
  } else if (varDecP(node.body)) {
    collection.push(node.body)

  } else if (node.type === 'FunctionDeclaration'
             || node.type === 'FunctionExpression'
             || node.type === 'ArrowFunctionExpression'
            ) {
    node.params.map(id => {
      const ident = (id.type === "RestElement") ? id.argument : id;
      console.log('Function Arg:', ident.name);
      collection.push(makeVarDecFromAss(
        { expression: { left: ident } }))
    })
    // console.log('Added Functions Args', collection, node)
  } else {
    findVarDecs(node.body, collection)
  }
  // console.log("Found Var Decs in", node.type, collection)
  return collection;
}
function mutateAssAST (AST) {
  walk.ancestor(AST, {
    ExpressionStatement (n, ancestors) {
      if (isAssP(n)) {
        const decs = []
        // console.log("Assigment", n, ancestors);
        ancestors.map(n => findVarDecs(n, decs));
        if (isGlobalAssP(n, decs)) {
          console.log("global", n.expression.left.name,'becoming a let');
          mutateAssIntoDec(n);
          // console.log(generate(n));
        }
      }
    },
    // Literal(_, ancestors) {
    //   console.log("This literal's ancestors are:", ancestors.map(n => n.type))
    // }
  })
}

function lexify(code) {
  const AST = (typeof code === 'string') ? acorn.parse(code) : code;
  const body = AST.body
  mutateAssAST(AST);
  return AST;
}

module.exports = lexify;


// function(content, _, _AST) {
//   const callback = this.async();
//   const AST = (typeof _AST === 'object') ? _AST : acorn.parse(content);
//   const ass = AST.body[0];
//   const lex = ass2var(ass);
//   const ret = returnVar(ass);
//   AST.body[0] = lex;
//   AST.body.push(ret);
//   const body = wrapFnExp(AST.body);
//   AST.body = [body];

//   // console.log(acorn.parse('() => {return foo}').body[0].expression.body.body);
//   // console.log(acorn.parse('(RST) => { var foo = 1; return foo };').body[0]);
//   callback(null, generate(AST), null, AST);
// }
