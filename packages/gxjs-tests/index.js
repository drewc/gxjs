const RTS = require('gambit-scheme')

globalThis.gambitRTS = RTS;
console.log("Have RTS?:", typeof RTS === 'function');

require('./gambit-scheme-auto.js')
const thisIsGambit = require('./gambit-scheme-usage.js')
const thisIsGerbil = require('./gerbil-scheme-usage.js')
require('./gerbil-scheme-auto.js')
const testUniversal = require('gambit-loader!./universal.scm')

console.log("Loaded universal.scm", testUniversal);

testUniversal()

const sameRTS = require('gerbil-scheme')

console.log("Have same RTS?:", RTS === sameRTS);

const testGerbilLoader = require('./gerbil-loader.js');

testGerbilLoader();

const testGerbilGambc = require('gerbil-loader!./test-gx-gambc.ss')

testGerbilGambc();

const GxJSRTS = require ('gxjs')
console.log("Have same GxJS = RTS?:", RTS === GxJSRTS);

const testGxJS = require('gerbil-loader!./test-gxjs.ss')

testGxJS();

require('./test-gxjs.js')

const GxJSLoader = require('./gxjs-loader.js')

GxJSLoader();

const gambitGxJSLoader = require('./gxjs-gambit-loader.js')

gambitGxJSLoader(1024);

const diffRTS = require('./gxjs-link-loader.js')

const answer = require('./gxjs-link-use-runtime.js')

console.log( 'Life? Universe? everything?', answer);
// const failure = require('gxjs-loader!./fail-gsc.ss')

module.exports = RTS
