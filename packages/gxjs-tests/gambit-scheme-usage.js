const RTS = require('gambit-scheme');

const thisIsGambit = require('gambit-loader!./gambit-scheme-usage.scm');

thisIsGambit(0.42);

module.exports = RTS;
