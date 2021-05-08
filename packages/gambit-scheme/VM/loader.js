const { execSync, execFileSync} = require("child_process");
const pty = require('node-pty');
const fs = require('fs');
const path = require('path');
const mktemp = require('mktmp');
const which = require('which');
const find = require('find');
const { getOptions } = require('loader-utils')

const { ensureAST, generate, walk } = require('./syntax.js')
const lexicalize = require('./lexicalize.js')
const { minify } = require('terser');

function GambitVMLoader () {
  const callback = this.async()
  const filePath = this.resourcePath;
  const fileExt = path.extname(filePath);
  const fileName = path.basename(filePath, fileExt);
  const cwd = path.dirname(this.resourcePath);
  const options = (() => {
    if (typeof this.getOptions === 'function') {
      return this.getOptions()
    } else {
      return getOptions(this)
    }
  })();


  const devel = this.mode === 'development'
  const verbose = (v => this.mode === 'development' ||
                   (v !== undefined && !!v))(options['-v'])
  function log (...args) { if (verbose) { console.log(...args) } };

  log('Options for Gambit loader', options)

  const terser = (t => t === undefined ? true : t)(options.terser)
  const dashE = (e => e === undefined ? [] : ['-e', e])(options['-e']);

  let err = false;

  // First, we copy the needed libraries

  const libs = ["~~lib/_six/js.scm", "~~lib/_six/js#.scm",
                "~~lib/_six/six-expand.scm", "~~lib/_six/six-expand#.scm"]

  function cpLib(lpath) {
    if (err) { return }
    const source = execSync('gsc -e \'(display (path-expand "'+lpath+'"))\'',
                         { encoding: 'utf8', shell: true})
    const name = path.basename(source), dest = cwd+'/'+name;
    try {
      fs.copyFileSync(source, dest, 0)
    } catch(e) {
      err = e; console.error("Cannot copy ", source, e.message); return;
    }
    if (err) { return }
    log("Copied "+source+" to " +dest)
    fs.chmod(dest, 0700, e => {
      if (e) { err = e; console.error("Cannot chmod ", dest, e.message); return; }
    })
  }
  if (err) { callback(err); return }
  libs.map(cpLib)
  if (err) { callback(err); return }

  // Then make an "executable"

  const outputFile = fileName + '.js'
  const args = ['-target', 'js', '-exe',
                '-label-namespace', 'x',
                '-o', outputFile,
                ...dashE,
                filePath]

  const gsc = which.sync('gsc');

  const ptyProcess = pty.spawn(gsc, args, {
    name: 'xterm-color',
    cols: 80,
    rows: 30,
    cwd: cwd,
    env: process.env
  });

  let stdout = "";
  log("\n\n---------------\nIn", cwd)
  log('Running', gsc, args.join(' '));

  ptyProcess.on('data', function(data) {
    console.log(data); stdout = stdout+data;
  });

  ptyProcess.on('exit', function(status) {
    if (status !== 0) {
      err = new Error(`GxJS compilation failed for ${gsc} ${args.join(' ')}\n\n ${stdout}`)
      return
    }
    log("---------------\n Reading", outputFile)
    const source = fs.readFileSync(cwd + '/' + outputFile, {encoding: 'utf8'})
    let AST = ensureAST(source)

    log('Making it lexical', fileName + '.lex.js')
    AST = lexicalize(AST, true);

    const sourceCode = generate(AST);
    fs.writeFileSync(cwd + '/' + fileName + '.lex.js', sourceCode);
    if (verbose || !terser) {
      log("loading un-minifed file")
      callback(null, sourceCode)
    } else {
      console.warn('Terser Minify', fileName + '.min.js CAN TAKE A LONG LONG TIME')
      minify(sourceCode, { sourceMap: true }).then(r => {
        log("Minified! Into", r.code.length, "chars")
        const minFN = fileName + '.min.js'
        log('Writing', minFN, 'to', cwd)
        fs.writeFileSync(cwd + '/' + minFN, r.code);
        callback(null, r.code, r.map)
      })
    }
  });

  if (err) { callback(err); return }

  return;
}
module.exports = GambitVMLoader;
