const RTS = require('gerbil-scheme');

const thisIsGerbil = require('gerbil-loader!./gerbil-scheme-usage.ss');

thisIsGerbil('*');

module.exports = RTS;
