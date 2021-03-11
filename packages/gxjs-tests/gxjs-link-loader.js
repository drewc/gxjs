const RTS = require('gxjs-loader?-link&return=RTS&exports!./gxjs-link-loader-runtime.ss');
const extraRunTime = require('gxjs-loader?args=["RTS"]&RTS=false&call=false&exports!./gxjs-link-loader-runtime.ss');

extraRunTime(RTS);
console.log('New RTS:', RTS.glo, extraRunTime)

module.exports = RTS;
