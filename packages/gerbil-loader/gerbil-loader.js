const { loadGxJS } = require('gxjs-loader')
const path = require('path');
function gerbilLoader () {
  const callback = this.async()
  const rpath = this.resourcePath
  const options = this.getOptions();

  options.gxc = ((o) => o === undefined ? true : o === '' ? true : o)(options.gxc);
  const cwd = path.dirname(this.resourcePath);

  const RTSrequire = (req => req === undefined ? 'gerbil-scheme' : req)(options['RTS']);
  options.RTS = RTSrequire;

  const devVerbose = this.mode === 'development'
  const verbose = ((v) => v === undefined ? devVerbose : v)(options['-verbose']);
  options['-v'] = verbose
  function log (...args) { if (verbose) { console.log(...args) } };


  log("Loading Gerbil Scheme using gerbil-loader: ", rpath, options)

  loadGxJS(options, rpath, callback);



}

module.exports = gerbilLoader
