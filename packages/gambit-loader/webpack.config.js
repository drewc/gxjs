const TerserPlugin = require("terser-webpack-plugin");
const path = require('path');

module.exports = {
  mode: 'development',
 // mode: 'production',
  entry: './gambit-loader.js',
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'gambit-loader.js',
    library: 'GambitLoader',
    libraryTarget:'umd',
  },
  externals: {
    'gxjs-loader': {  commonjs2: 'gxjs-loader', commonjs: 'gxjs-loader' }
  },
  devServer: {
    contentBase: './dist',
    port: 8484
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
