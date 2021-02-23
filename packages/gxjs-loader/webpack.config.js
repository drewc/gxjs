const TerserPlugin = require("terser-webpack-plugin");
const path = require('path');

module.exports = {
  mode: 'development',
  // mode: 'production',
  entry: './gxjs-loader.js',
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'gxjs-loader.js',
    library: 'GxJSLoader',
    libraryTarget:'umd',
  },
  externals: {
    'node-pty': {  commonjs2: 'node-pty', commonjs: 'node-pty' }
  },
  optimization: {
    minimize: true,
    minimizer: [new TerserPlugin({})],
  },
   module: {
     rules: [
       {
         test: /\.js$/,
         exclude: /(node_modules|bower_components)/,
         use: {
           loader: 'babel-loader',
           options: {
             comments: false,
             presets: ['@babel/preset-env', 'minify'],
             //plugins: [["minify-mangle-names", { topLevel: true } ]]
         }
        }
       }
    ]
   },

  target: 'node'
};
