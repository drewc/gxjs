const path = require('path');
const webpack = require('webpack');


 const ignorePath = new webpack.ContextReplacementPlugin(
      /^\.$/,
      (data) => {
        delete data.dependencies[0].critical;
        return data;
      }
)

module.exports = {
  mode: 'production',
  entry: {'GambitScheme': './VM/GambitVM.js' },
  plugins: [ignorePath],
  output: {
    path: path.resolve(__dirname, './dist/'),
    filename: 'gambit-scheme.js',
    library: '[name]',
    libraryTarget: 'umd',
    globalObject: 'this'
  },
  externals: {
    'fs': 'commonjs2 fs',
    'posix': 'commonjs2 posix',
    'child_process': 'commonjs2 child_process',
    'buffer': 'commonjs2 buffer',
  },
  // optimization: {
  //   minimize: true,
  //   minimizer: [new TerserPlugin({})],
  // },
   module: {
     rules: [
       { test: /\.scm$/,
         use: [
          {
            loader: path.resolve('VM/loader.js'),
            options: {
              /* ... */
              terser: false,
              '-v': true
            },
          },
        ],
       },
    ]
   },
};
