const path = require('path')

const CopyWebpackPlugin = require('copy-webpack-plugin')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const MiniCssExtractPlugin = require('mini-css-extract-plugin')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')


module.exports = {
    entry: [
        './src/index.js',
        './src/sass/main.sass'
    ],

    output: {
        path: path.resolve(__dirname + '/dist'),
        filename: '[name].[chunkhash].js',
        publicPath: '/'
    },

    module: {
        rules: [
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader: process.env.NODE_ENV === 'production' ? 'elm-webpack-loader?verbose=true&optimize=true' : 'elm-webpack-loader?verbose=true'
            },
            {
                test: /\.(sass|scss|css)$/,
                loader: [
                    { loader: MiniCssExtractPlugin.loader },
                    'css-loader',
                    'sass-loader'
                ]
            },
            {
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'url-loader?limit=10000&mimetype=application/font-woff'
            },
            {
                test: /\.(ttf|eot|svg|png)(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: 'file-loader'
            }
        ],

        noParse: /\.elm$/
    },

    plugins: [
        new HtmlWebpackPlugin({
            title: 'Registry',
            template: 'src/index.ejs'
        }),
        new MiniCssExtractPlugin({
            filename: '[name].[chunkhash].css',
            allChunks: true
        }),
        new OptimizeCssAssetsPlugin({
            cssProcessorPluginOptions: {
                preset: ['default', { discardComments: { removeAll: true } }]
            }
        }),
        new CopyWebpackPlugin([
            { from: 'src/img', to: 'img' },
        ])
    ],

    devServer: {
        inline: true,
        stats: { colors: true },
        historyApiFallback: true
    }
}
