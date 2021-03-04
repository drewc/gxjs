const { execSync, execFileSync} = require("child_process");
const pty = require('node-pty');
const fs = require('fs');
const path = require('path');
const mktemp = require('mktmp');
const which = require('which');
const find = require('find');

function spawn_gsc(opts, callback, ...files) {

  const cwd = opts.cwd;
  const gsc = which.sync('gsc');

  const verbose = ((v) => v === undefined ? false : v)(opts['-v']);
  function log (...args) { if (verbose) { console.log(...args) } };

  const temp = (t => t === undefined ? mktemp.createDirSync('Gambit-XXXX') : t)(opts.temp);

  const libdir = execSync("gxi -e '(print (car (library-load-path)))'",
                         { encoding: 'utf8', shell: true})
  const gxGambcSharp = (o => {
    if (o === true) {
      return find.fileSync('gx-gambc#.scm', libdir)[0];
    } else {
      return false
    }
  })(opts.gxGambcSharp);

  // console.log("\n\n\n\n SHARTP!", gxGambcSharp, ...files, opts.gxGambcSharp);

  const incGxGambcSharp = gxGambcSharp ? ['-e', `(include "${gxGambcSharp}")`] : []


  const link = (l => l === undefined ? false : (l !== false))(opts.['-link']);
  const outputType = link ? '-link' : '-c'
  const linkChar = link ? '_' : '';

  const args = ['-target', 'js',
                '-prelude', '(include "~~lib/_gambit#.scm")',
                '-repr-module', 'class',
                '-namespace', '__GxJS_',
                '-o', temp, outputType,
                ...incGxGambcSharp,
                ...files]

  const names = files.map(f => path.basename(f, path.extname(f)));

  let outputs = names.map(name => `${temp}/${name}${linkChar}.js`)
  if (link) { outputs = [outputs[outputs.length - 1]]}



  var ptyProcess = pty.spawn(gsc, args, {
    name: 'xterm-color',
    cols: 80,
    rows: 30,
    cwd: cwd,
    env: process.env
  });

  let stdout = "";

  log('Running', cwd, '$', gsc, args.join(' '));

  ptyProcess.on('data', function(data) {
    console.log(data); stdout = stdout+data;
  });

  ptyProcess.on('exit', function(status) {
    let error = false;
    if (status !== 0) {error =  new Error(`GxJS compilation failed for ${gsc} ${args.join(' ')}\n\n ${stdout}`) }
    callback({root: temp, gsc, status, error, args, files, names, outputs})
  });

  return ;

}

module.exports = spawn_gsc;
