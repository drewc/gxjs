const { execSync } = require("child_process");
const fs = require('fs');
const path = require('path');

const gsc_args = "-target js -prelude '(include \"~~lib/_gambit#.scm\")' -repr-module class -namespace '__GxJS_' "


function exec_gsc(cwd, ...args) {
  // $ gsc -help
  // => usage: gsc [options] [module-or-file...]

  // any string that does not start with '-' is the start of the files.

  function isFile(arg) {
    return (typeof arg === 'string' && arg.charAt(0) !== '-');
  };

  var options = false, files = false;
    for (var i = 0, j = args.length; i < j && files === false; i++) {
      var arg = args[i];
      if (isFile(arg)) {
        options = [...args].slice(0,i);
        files = args.slice(i);
       }
    };

  if (!files) { throw new Error('No file to compile in ' + args) };

  const is_link = !!options.find(o => o === '-link');
  const dash_o = options.find(o => o.startsWith('-o '));

  function scm2jsName(fpath, dash_oName = false, link = false) {
    const fdir = path.dirname(fpath);
    const fext = path.extname(fpath);
    const fname = path.basename(fpath, fext);
    // If this is a link file gsc starts with _ automagically.
    return path.normalize(
      fdir + '/' + (link ? '_' : '') + (dash_oName ? dash_oName : (fname + '.js'))
    );
  }

  // If there is (no -o OR (files.length > 1) then all the files become JS files
  const first_paths = (!dash_o || (files.length > 1 || is_link))
        ? files.map( n => scm2jsName(n, false, false)) : [];

  // If there's a -o and (files.length = 1 OR is_link) that file exists
  const dash_o_paths = (dash_o && (is_link || files.length === 1)) ? [ scm2jsName(files[0], dash_o.slice(3)) ] : [];

  // if is_link and (!dash_o) then there's a link file.
  const link_paths = (is_link && !dash_o)
        ? [scm2jsName(files[files.length - 1], false, true)] : [];

  const link = (is_link) ? [...dash_o_paths, ...link_paths][0] : false;

  const output = [ ...dash_o_paths, ...link_paths, ...first_paths ]

  const _cmd = 'gsc ' + gsc_args + options.join(' ') +
        ' ' + files.join(' ') + ''

  // For some reason `gsc` does not display errors for non-interactive shells.
  // installing unbuffer makes it think otherwise.
  const unbuffer = (() => {
    try { return execSync('which unbuffer', { encoding: 'utf8' })
          .replace(/(\r\n|\n|\r)/gm,"")
        } catch(_) { return false } })();

  const cmd = (() => {
   if (unbuffer) {
     return unbuffer + ' ' + _cmd
   } else { return _cmd + ' sh -c "$_gsc"' }
  })()


  var error = false;

  console.log('trying', cmd)

  try {

    let comp = execSync(cmd, { cwd, encoding: 'utf8', })
 //   console.log('comp:', comp)
  } catch(e) {
     const message = `compilation failed for ${cmd} : \n \n  ${e.stderr} ${e.stdout}`
     error = new Error(message);
     console.error(message);
  }

  return { _cmd, cwd, options, input: files, output, link, error} ;
};


module.exports = exec_gsc;
