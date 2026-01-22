import type IForkTsCheckerWebpackPlugin from 'fork-ts-checker-webpack-plugin'
import { DefinePlugin } from 'webpack'
const ForkTsCheckerWebpackPlugin: typeof IForkTsCheckerWebpackPlugin = require('fork-ts-checker-webpack-plugin')
export const plugins = [
  new ForkTsCheckerWebpackPlugin({
    logger: 'webpack-infrastructure',
  }),
  new DefinePlugin({
    'process.env.TELEMETRY_WRITE_KEY': JSON.stringify(process.env.TELEMETRY_WRITE_KEY),
  }),
]