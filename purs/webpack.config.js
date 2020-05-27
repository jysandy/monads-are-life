const path = require("path");
const CleanWebpackPlugin = require("clean-webpack-plugin");
const UglifyJsPlugin = require("uglifyjs-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const ResourceHintWebpackPlugin = require("resource-hints-webpack-plugin");
const ScriptExtHtmlWebpackPlugin = require("script-ext-html-webpack-plugin");
const SubresourceIntegrityWebpackPlugin = require("webpack-subresource-integrity");
const ManifestPlugin = require("webpack-manifest-plugin");
const { BundleAnalyzerPlugin } = require("webpack-bundle-analyzer");

const outputPath = "build";

const isProd = ({ mode }) => mode === "production";

module.exports = (_env, options) => ({
  entry: {
    main: "./src/index.js",
  },
  resolve: {
    alias: {
      'node_modules': path.join(__dirname, 'node_modules')
    }
  },
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, outputPath),
    crossOriginLoading: "anonymous"
  },
  optimization: {
    minimizer: [
      new UglifyJsPlugin({
        cache: true,
        parallel: true
      }),
      new OptimizeCSSAssetsPlugin({
        cssProcessorOptions: {
          safe: true
        }
      })
    ],
    moduleIds: isProd(options) ? "size" : "named",
    splitChunks: {
      chunks: "all",
      cacheGroups: {
        styles: {
          name: "styles",
          test: /\.(css)$/,
          chunks: "all",
          enforce: true
        },
        react: {
          name: "react",
          test: /[\\/]node_modules\/(react|react-dom)[\\/]/,
          chunks: "all",
          priority: -5
        },
        purs: {
          name: "purs",
          test: /[\\/]output[\\/]/,
          minChunks: 2,
          priority: -5
        },
        default: {
          minChunks: 2,
          priority: -20
        }
      }
    }
  },
  plugins: [
    new CleanWebpackPlugin(["build"]),
    new MiniCssExtractPlugin(),
    new HtmlWebpackPlugin({
      template: "src/index.html"
    }),
    new ResourceHintWebpackPlugin(),
    new ScriptExtHtmlWebpackPlugin({
      defaultAttribute: "async"
    }),
    new SubresourceIntegrityWebpackPlugin({
      hashFuncNames: ["sha256", "sha384"],
      // this is here as an example, comes at a perf cost so
      // we probably only want to use it for 3rd party scripts
      enabled: false // isProd(options)
    }),
    new ManifestPlugin(),
    new BundleAnalyzerPlugin({
      analyzerMode: isProd(options) ? "static" : "disabled",
      openAnalyzer: false
    })
  ],
  module: {
    rules: [
      {
        test: /\.js$/,
        loader: "source-map-loader",
        exclude: /node_modules|bower_components/
      },
      {
        test: /\.purs$/,
        exclude: /node_modules/,
        use: {
          loader: "purs-loader",
          options: {
            bundleOutput: "output/bundle.js",
            bundle: isProd(options),
            warnings: true,
            spago: true,
            watch: options.watch,
          }
        }
      },
      {
        test: /\.css$/i,
        use: [MiniCssExtractPlugin.loader, 'css-loader']
      },
      {
        test: /\.(woff(2)?|ttf|eot|svg)(\?v=\d+\.\d+\.\d+)?$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              outputPath: 'fonts',
              name: '[name].[ext]',
              esModule: false
            }
          }
        ]
      },
      {
        test: /\.jpg?$/,
        use: [
          {
            loader: 'file-loader',
            options: {
              outputPath: 'images',
              name: '[name].[ext]',
              esModule: false
            }
          }
        ]
      }
    ]
  }
});
