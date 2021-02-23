const TerserPlugin = require("terser-webpack-plugin");
const path = require('path');


module.exports = {
  // mode: 'development',
  mode: 'production',
  entry: './gxjs.js',
  // entry: {
  //   // loaderTest: 'loader/test.js',
  //   // 'test/loaders': './test/loaders.js',
  //   gxjs: './gxjs.js',
  //   //"univ/vector": { import: './gambit/univ/vec.js', dependOn: 'gxjs' },
  //   //'gambit/univ': 'gxjs-lex!gxjs-min!gsc!./gambit/univ.scm',
  //   // rts: './rts.js',
  //   // 'univ/test': { import: './test/utest.js', dependOn: 'gxjs' }
  // },
  output: {
    chunkFilename: '[name].bundle.js',
    path: path.resolve(__dirname, './dist'),
    filename: 'gxjs.js',
    library: 'GxJS',
    libraryTarget:'umd',
    //libraryExport: 'default',
    // globalObject: 'this',
  },
  resolve: {
    alias: {
      'gxjs-univ': path.resolve(__dirname, 'gxjs-univ.js')
    },
    alias: {
      gxjs$: path.resolve(__dirname, 'gxjs.js')
    }
  },
  resolveLoader: {
    modules: [
      'node_modules',
      path.resolve(__dirname, 'loaders')
    ]
  },
  devServer: {
    contentBase: './dist',
    port: 8484
  },
  optimization: {
    minimize: true,
    minimizer: [new TerserPlugin({
      // extractComments: false,
      // terserOptions: {
      //   format: {
      //     comments: false,
      //   },
      //},
    })],
  },
   module: {
     rules: [
       {
        test: /\.html$/,
        loader: 'html-loader',
       },
       {
         test: /foo\.js$/,
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

  // stats: {
  //   logging: 'verbose'
  // },
  target: 'web'
};
