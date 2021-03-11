const { loadGxJS } = require('gxjs-loader')
const path = require('path');
function gambitLoader () {
  const callback = this.async()
  const rpath = this.resourcePath
  const options = this.getOptions();

  options.gxc = false;
  options.gxGambcSharp = false;

  const cwd = path.dirname(this.resourcePath);

  const RTSrequire = (req => req === undefined ? 'gambit-scheme' : req)(options['RTS']);
  options.RTS = RTSrequire;

  const devVerbose = this.mode === 'development'
  const verbose = ((v) => v === undefined ? devVerbose : v)(options['-v']);
  options['-v'] = verbose
  function log (...args) { if (verbose) { console.log(...args) } };


  log("Loading Gambit Scheme using gambit-loader: ", rpath, options)

  loadGxJS(options, rpath, callback);



}

module.exports = gambitLoader
