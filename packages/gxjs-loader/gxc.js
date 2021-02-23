const pty = require('node-pty');
const fs = require('fs');
const path = require('path');
const mktemp = require('mktmp');
const which = require('which');
const find = require('find');

function spawn_gxc(cwd, callback, ...files) {
  const gxc = which.sync('gxc');
  const temp = mktemp.createDirSync('Gerbil-XXXX');
  const args = ['-v', '-d', temp, '-s', '-S', ...files]
  const names = files.map(f => path.basename(f, path.extname(f)));
  const scmNames = names.map(name => `${name}__0.scm`)


  var retval, outputs, error = false;
  var ptyProcess = pty.spawn(gxc, args, {
    name: 'xterm-color',
    cols: 80,
    rows: 30,
    cwd: cwd,
    env: process.env
  });

  let stdout = "";

  // console.log('Running', cwd, '$', gxc, args.join(' '));
  ptyProcess.on('data', function(data) {
    // console.log(data);
    stdout = stdout+data;
  });

  ptyProcess.on('exit', function(status) {
    // console.log("gxc completed, ", status)
    let error = false;

    if (status !== 0) {
      error = new Error(`GxJS compilation failed for ${gxc} ${args.join(' ')}\n\n ${stdout}`)
    } else {
      // If there's a package this file may be elsewhere so find it
      const out = scmNames.map(x => find.fileSync(x, temp)[0]);
      const _rep = /__0\.scm$/


      // console.log("gxc completed, ", status)
      // At this point the file we want to `gsc` is now in *___0.scm
      // rename them! This makes `gsc` have the right module names`
      outputs = out.map(o => o.replace(_rep, ".scm"));
      out.map(path => fs.renameSync(path, path.replace(_rep, ".scm")))
    };

    // console.log("gxc completed, ", status)
    callback({root: temp, status, error, args, files, names, outputs});
  });

  return ;


}

module.exports = spawn_gxc;
