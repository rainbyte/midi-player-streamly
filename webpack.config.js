const path = require('path');

module.exports = {
    mode: 'production',
    entry: './src/Main.js',
    resolve: {
        alias: {
            svelte: path.resolve('node_modules', 'svelte')
        },
        extensions: ['.mjs', '.js', '.svelte'],
        mainFields: ['svelte', 'browser', 'module', 'main']
    },
    module: {
        rules: [
            {
                test: /\.(html|svelte)$/,
                exclude: /node_modules/,
                use: 'svelte-loader'
            }
        ]
    },
    output: {
        path: __dirname + '/out',
        publicPath: '/',
        filename: 'bundle.js'
    }
};