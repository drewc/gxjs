const TerserPlugin = require("terser-webpack-plugin");
const path = require('path');

module.exports = {
  // mode: 'development',
  mode: 'production',
  entry: { 'gambit-scheme': './gambit-scheme.js' },
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: '[name].js',
    library: 'gambitScheme',
    libraryTarget: 'umd',
    globalObject: 'this'
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
};
