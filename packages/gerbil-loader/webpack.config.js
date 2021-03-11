const TerserPlugin = require("terser-webpack-plugin");
const path = require('path');

module.exports = {
  // mode: 'development',
  mode: 'production',
  entry: './gerbil-loader.js',
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'gerbil-loader.js',
    library: 'GerbilLoader',
    libraryTarget:'umd',
  },
  externals: {
    'gxjs-loader': {  commonjs2: 'gxjs-loader', commonjs: 'gxjs-loader' }
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
