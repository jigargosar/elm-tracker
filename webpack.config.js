const HtmlWebpackPlugin = require('html-webpack-plugin')
const { CleanWebpackPlugin } = require('clean-webpack-plugin')

/*

npm init -y
npm i -D webpack webpack-cli webpack-dev-server html-webpack-plugin style-loader css-loader elm-webpack-loader
npm i ramda tachyons
shx rm elm.json
elm init
git init
git add .
git commit -m "Initial Commit"

*/

// https://webpack.js.org/configuration/
module.exports = (_, config) => {
  const isProd = config.mode === 'production'
  return {
    entry: './src/index.js',
    output: {
      publicPath: '/',
    },
    resolve: {
      extensions: ['.js', '.elm'],
    },
    plugins: [
      new CleanWebpackPlugin(),
      new HtmlWebpackPlugin({ template: './src/index.html' }),
    ],

    module: {
      rules: [
        {
          include: /\.elm/,
          use: [
            //'elm-hot-webpack-loader',
            {
              loader: 'elm-webpack-loader',
              options: { optimize: isProd, debug: !isProd },
            },
          ],
        },
        {
          include: /\.css/,
          use: [
            'style-loader',
            'css-loader',
            // POST CSS
            // { loader: 'css-loader', options: { importLoaders: 1 } },
            // {
            //   loader: 'postcss-loader',
            //   options: {
            //     ident: 'postcss',
            //     plugins: [require('tailwindcss')],
            //   },
            // },
          ],
        },
      ],
    },
    // https://webpack.js.org/configuration/stats/
    // stats: 'errors-warnings',
    stats: {
      children: false,
      modules: false,
    },
    devtool: isProd ? 'source-map' : 'eval-source-map',
    devServer: {
      historyApiFallback: true,
      hot: true,
      overlay: true,
    },
  }
}
