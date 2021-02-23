const { ensureAST, generate, walk } = require('./syntax.js')

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
    log("Minimize", name, "to", nsym);
    return minsym(name);
  } else {
    return sym }
}

function newEnvironment () {
  n = -1;
}

let verbose = false;

function log (...args) { if (verbose) { console.log(...args) } };

function minimizeGambitNamespace (source, options = { verbose: false }) {
  const AST = ensureAST(source);

   verbose = ((v) => v === undefined ? false : v)(options['verbose']);

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

  return AST;
}

module.exports = minimizeGambitNamespace
module.exports.newEnvironment = newEnvironment;
