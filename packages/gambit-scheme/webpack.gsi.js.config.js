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
  // mode: 'development',
  target: 'node',
  entry: {'gsi': './gsi.js' },
  plugins: [ignorePath,
            new webpack.BannerPlugin({ banner: "#!/usr/bin/env node", raw: true })],
  output: {
     path: path.resolve(__dirname, './dist/bin'),
     filename: '[name].js',
  },
  externals: {
  //   'fs': 'commonjs2 fs',
    'posix': 'commonjs2 posix',
  //   'child_process': 'commonjs2 child_process',
  //   'buffer': 'commonjs2 buffer',
  // },
  // optimization: {
  //   minimize: true,
  //   minimizer: [new TerserPlugin({})],
  },
   module: {
     rules: [
       { test: /\.scm$/,
         use: [
          {
            loader: path.resolve('VM/loader.js'),
            options: {
              '-exe' : true,
            },
          },
        ],
       },
    ]
   },
};
