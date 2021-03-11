const RTS = require('gambit-scheme')
const GxJSGambJS = require('gxjs-loader?args=["RTS"]&RTS=false&call=false&exports!./gxjs-gambjs.scm');

GxJSGambJS(RTS);
module.exports = RTS;
