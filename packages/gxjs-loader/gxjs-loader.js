const spawn_gsc = require ('./gsc.js')
const spawn_gxc = require ('./gxc.js')
const path = require('path');
const fs = require('fs');
const { getOptions } = require('loader-utils')
const { ensureAST, generate } = require('./syntax.js')
const minimizeGambitNamespace = require ('./namespace-min.js')
const lexify = require('./lexify.js')
const wrapArrow = require('./wrap-arrow.js')
const wrapCallExpression = require('./call-wrapper.js')
const prependConstRTS = require('./const-RTS.js')
const wrapModuleExports = require('./module-exports.js')

const changeModInit = require('./module-registry-init.js')

function allFiles (things) { // files are strings
  return things.filter(x => typeof x === 'string');
};

function getCallback (things) { //callbacks are functions
  return things.filter(x => typeof x === 'function')[0];
}

function getCwd (options, files) {
  const cwd = options.cwd
  return cwd !== undefined ? cwd : path.dirname(files[files.length - 1]);
}

function callGerbil(options, ...filesAndCallback) {

  const files = allFiles(filesAndCallback);
  const callback = getCallback(filesAndCallback);
  const cwd = getCwd(options, files);

  console.log('callback', callback)

  const verbose = ((v) => v === undefined ? false : v)(options['-v']);
  function log (...args) { if (verbose) { console.log(...args) } };

  log("calling gxs, ", cwd,  ...files, options)
  spawn_gxc(cwd, gerbil => callback(gerbil), ...files);

  return;
}

/* Gambit

   could be -link
*/

function callGambit(options, ...filesAndCallback) {

  const files = allFiles(filesAndCallback);
  const callback = getCallback(filesAndCallback);
  const cwd = getCwd(options, files);

  const verbose = ((v) => v === undefined ? false : v)(options['-v']);
  function log (...args) { if (verbose) { console.log(...args) } };

  const isLink = options['-link'];

  log("calling Gambit, ", ...files, options)

  function callCallback (gambit) {
    if (!isLink) {
      return callback(gambit);
    } else {
      return spawn_gsc(
        {
          cwd: gambit.root,
          temp: gambit.root,
          '-link': true,
          gxGambcSharp: options.gxGambcSharp
        },
        linkGambit => {
          if (linkGambit.error) {
            gambit.error = linkGambit.error;
            return callback(gambit);
          }

          gambit.link = linkGambit.outputs[0];
          return callback(gambit)


        },
        ...gambit.outputs
      );

    }
  };

  spawn_gsc(
    {
      cwd: cwd,
      gxGambcSharp: options.gxGambcSharp
    },
    callCallback,
    ...files
  );

  return;
}

function loadGxJS(options, ...filesAndCallback) {

  const files = allFiles(filesAndCallback);
  const callback = getCallback(filesAndCallback);
  const cwd = getCwd(options, files);

  const RTSrequire = (req => req === undefined ? 'gxjs' : (req === 'false' ? false : req))(options['RTS']);
  options.RTS = RTSrequire;


  const arrowArgs = ((args) => {
    return (typeof args === 'string') ? JSON.parse(args) : args
  })((options.args !== undefined) ? options.args : false);
  const arrowReturn = ((options.return !== undefined) ? options.return : false);
  const arrowCall = (c => {
   if (c !== undefined) {
     if (c === 'false') { return false } else {return c}
   } else {
     return true
   }
  })(options.call);

  const verbose = ((v) => v === undefined ? false : v)(options['-v']);
  function log (...args) { if (verbose) { console.log(...args) } };

  // only run gsc?
  const gscOnly = ((o)  => {
    const gsc = path.extname(files[0]) === '.scm';
    const runit = ((gsc && o.gxc === undefined) || o.gxc === false);

    log("Run gsc only?", runit, '!gxc', o.gxc === false, 'gsc', gsc);
    return runit;
  })(options)

  const gxGambcSharp = (o => o === undefined ? true : o === '' ? true : o)(options.gxGambcSharp);
  options.gxGambcSharp = gxGambcSharp;
  log('\n\n\gx-gambc#?', options.gxGambcSharp);

  const addModuleExports = (e => (e === undefined) ? false : (e === '') ? true : e)(options.exports);

  const gambitLink = (l => (l === undefined) ? false : (l === '') ? true : l)(options.['-link'])
  options.['-link'] = gambitLink;


  function rm_rf (dir) {
    if (dir) {
      fs.rmdir(dir, { recursive: true }, () => {
        log("Folder", dir, "for output of", files, "Deleted.");
      });
    }
  };

  log('\n-----------------------\nLoading: ', files, options)

  log("Run gsc only?", gscOnly)
  const runGxc = (() => {
    if (!gscOnly) {
      return callGerbil
    } else {
      return function (...args)  {
       log("Not running gxc for", ...files)
        getCallback(args)({ root: false, error: false, outputs: files });
      };
    }
  })();


  // First, unless specified otherwise, we compile Gerbil to Gambit
  runGxc(options, ...files, (gerbil) => {
    log('Compiled Gerbil?:', gerbil)
    if (gerbil.error) {
      rm_rf(gerbil.root);
      callback(gerbil.error); return;
    }

    // Now take those Gambit files and compile to JavaScript

    const gscFiles = gerbil.outputs

    callGambit(options, ...gscFiles, (gambit) => {
      log("Compiled Gambit? :", gambit)
      if (!!gambit.error) {
        rm_rf(gambit.root);
        callback(gambit.error) ; return;
      }

      // Awesome, we now have some javascript files. That allows us to read the
      // file and delete all the temporary things.

      let error = false;

      let AST = false;
      let ASTs = ( files => {
        if (!error) {
          return files.map (path => {
            if (!error) {
              // read the file as a string
              const str = (() => {
                log('\nReading AST from', path)
                try {
                  return fs.readFileSync(path, {encoding: 'utf8'})
                } catch (e) { error = e }
              })();

              if (error) {return};

              // make it into an AST
              const AST = (() => {
                try {
                  return ensureAST(str)
                } catch(e) {error = e }
              })();

              if (!error) {
                return AST;
              }
            }
          })
        }
      })(gambit.outputs);

      let linkAst = false;
      if (gambit.link !== undefined) {
        const linkFile = fs.readFileSync(gambit.link, {encoding: 'utf8'})
        // Change the module init to not need `_gambit`
        log('Changing mod registry init in', gambit.link)
        linkAST = changeModInit(
          ensureAST(linkFile),
          { verbose: verbose }
        );
      }

      log('Have', ASTs.length, 'ASTs', 'link?', gambitLink)
      if (!error) {
        try {
          // Minimize things in "__GxJS__.*" namespace.
          // i.e.:  __GxJS.bb1_runtime_23_.name => a.b.name
          ASTs = ASTs.map( ast => minimizeGambitNamespace(ast, { verbose: verbose }));

          if (gambit.link !== undefined ) {
            ASTs = [linkAST, ...ASTs]

          }

          // No Undeclared Globals Allowed!!
          ASTs = ASTs.map( ast => lexify(ast, { verbose: verbose }));

          // if there are many ASTs make it into one.
          log('Concat ASTs into one AST');
          ASTs.map(ast => {
            return !AST ? AST = ast : ast.body.map(x => AST.body.push(x))
          })

          // Wrap the entire concatenated file/AST in an ArrowFunctionExpression
          AST = wrapArrow(AST, arrowArgs, arrowReturn, {verbose: verbose});

          log("")

          // If we call the arrow, now's the time to wrap it that way
          if (arrowCall) {
            AST = wrapCallExpression(AST, arrowCall, { verbose: verbose })
          }

          // regardless, do we want to add a `module.exports =` before that form?
          if (addModuleExports) {
            AST = wrapModuleExports(AST);
          }


          // Most modules need a runtime system. By default ours is "gxjs"
          if (RTSrequire && (!gambitLink || gambit.link === undefined)) {
            AST = prependConstRTS(AST, RTSrequire, { verbose: verbose })
          }


        } catch(e) {error = e};


      }

      rm_rf(gambit.root);
      rm_rf(gerbil.root);
      log('Generated source', generate(AST))
      if (error) {
        callback(error)
      } else {
        callback(null, generate(AST));
      }
    });
  })

  return;

}

function gxjsLoader () {
  const callback = this.async()
  const rpath = this.resourcePath

  const getO = () => {
    if (typeof this.getOptions === 'function') {
      return this.getOptions()
    } else {
    return getOptions(this)
    }
  }

  const options = getO();

  console.log('options', options, getOptions(this))

  const cwd = path.dirname(this.resourcePath);

  const RTSrequire = (req => req === undefined ? 'gxjs' : req)(options['RTS']);
  options.RTS = RTSrequire;

  const devVerbose = this.mode === 'development'
  const verbose = ((v) => v === undefined ? devVerbose : v)(options['-v']);
  options['-v'] = verbose
  function log (...args) { if (verbose) { console.log(...args) } };

  log("\n\n**** Loading GxJS using gxjsLoader: ", rpath, options)

  loadGxJS(options, rpath, callback);


}

module.exports =  gxjsLoader
module.exports.loadGxJS = loadGxJS;
