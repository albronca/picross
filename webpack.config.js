var path = require("path");

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: 'main.js',
  },

  module: {
    rules: [
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader:  'elm-webpack-loader',
          options: {}
        }
      },
    ],
  },

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
