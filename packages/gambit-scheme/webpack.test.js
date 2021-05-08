const path = require('path');

module.exports = {
  mode: 'development',
  target: 'web',
  entry: './test/index.js',
  output: {
    path: path.resolve(__dirname, './test/dist'),
    filename: 'test.js',
  },
  devServer: {
    contentBase: './dist',
    port: 8484
  },
};
