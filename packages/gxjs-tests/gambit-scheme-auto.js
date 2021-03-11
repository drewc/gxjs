const RTS = require('./gambit-scheme-usage.js');
const sameRTS = require('gambit-loader!./gambit-scheme-auto.scm');

console.log('Same Runtime?', RTS === sameRTS);
