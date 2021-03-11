import RTS from './gerbil-scheme-usage.js'
import sameRTS from 'gerbil-loader!./gerbil-scheme-auto.ss';

console.log('Same Runtime?', RTS === sameRTS);
