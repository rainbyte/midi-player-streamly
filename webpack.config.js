const webpack = require('webpack');

module.exports = {
    mode: 'production',
    entry: './src/Main.js',
    module: {
        rules: [
            {
                test: /\.(js|jsx)$/,
                exclude: /node_modules/,
                use: {
                    loader: 'babel-loader',
                    options: {
                        presets: ['@babel/preset-react']
                    }
                }
            },
            {
                test: /\.css$/,
                use: ['style-loader', 'css-loader']
            }
        ]
    },
    output: {
        path: __dirname + '/out',
        publicPath: '/',
        filename: 'bundle.js'
    }
};