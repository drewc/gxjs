const RTS = require('./runtime.js');
require('gxjs-loader?RTS=./runtime.js&gxGambcSharp=false!./prims.scm');

module.exports = RTS;
