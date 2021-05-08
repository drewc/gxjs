const acorn = require("acorn")
const walk = require("acorn-walk")
const { generate } = require('astring')

const ids = {};
function findIds(AST) {
  walk.simple(AST, {
   Identifier(id) { ids[id.name] = true }})
}

const syms = {};
const chars = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
const limit = chars.length;

var n = -1;

function gensym () {
  n++;
  if (n < limit) {
    return chars[n]
  };

  function make_gensym_prefix(firstn) {
    if (firstn > limit) {
      return make_gensym(firstn) + make_gensym(firstn - limit)
    } else {
      return chars[(firstn - 1)]
    }

  };

  const firstn = parseInt(n / limit);
  const sn = n - (limit * firstn);
  const secondn = chars[sn];
  // console.log(n, limit, firstn, sn, secondn)
  return make_gensym_prefix(firstn) + secondn;

};

function minsym(name) {
  const sym = syms[name];
  if (sym === undefined) {
    nsym = gensym();
    if (ids[nsym] !== undefined) {
     return minsym(name)
    }
    syms[name] = nsym;
    return minsym(name);
  } else { return sym }
}


module.exports = function(source, resetN = false) {
  const AST = (typeof source === 'string') ? acorn.parse(source) : source;

  if (resetN) {
    n = -1;
  };

   walk.simple(AST, {
      MemberExpression(exp) {
        const oname = (exp.object.type === 'Identifier') ? exp.object.name : '';
        const pname = exp.property.name;
        if (oname !== undefined && oname.startsWith('__GxJS_') && pname.startsWith('bb')) {
          exp.property.name = minsym(pname);
        }
      },
     Identifier(node) { // first pass just make a gen symbol to keep it short
       if (node.name.startsWith('__GxJS_')) { minsym(node.name) }
     },
    })
  

  walk.simple(AST, {
    AssignmentExpression(n) {
      const iname = (n.left.type === 'Identifier') ? n.left.name : '';
      if (iname !== undefined && iname.startsWith('__GxJS_')) {
        n.left.name = minsym(iname);
      }
    },
    Identifier(node) {
      // now minimize that symbol as we no longer need it for the member
      // expression
      if (node.name.startsWith('__GxJS_')) { node.name = minsym(node.name) }
    },
  })

  // console.log('minify:', generate(AST))

  return AST;
}
