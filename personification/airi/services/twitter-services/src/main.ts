import type { Context } from 'node:vm'
import type { AiriAdapter } from './adapters/airi-adapter'
import type { MCPAdapter } from './adapters/mcp-adapter'
import process from 'node:process'
import { useAdapter } from './adapters/adapter'
import { useConfigManager } from './config'
import { initBrowser, useContext } from './core/browser/context'
import { useTwitterAuthServices } from './core/services/auth'
import { initLogger, logger } from './utils/logger'
async function cleanup(adapters: { airi?: AiriAdapter, mcp?: MCPAdapter }, browserCtx: Context) {
  logger.main.log('Stopping Twitter service...')
  if (adapters.mcp) {
    await adapters.mcp.stop()
    logger.main.log('MCP adapter stopped')
  }
  if (browserCtx) {
    await browserCtx.close()
    logger.main.log('Browser closed')
  }
  logger.main.log('Twitter service stopped')
}
function setupShutdownHooks(adapters: { airi?: AiriAdapter, mcp?: MCPAdapter }, browserCtx: Context) {
  const handleShutdown = async (signal: string) => {
    logger.main.log(`Received ${signal} signal...`)
    await cleanup(adapters, browserCtx)
    process.exit(0)
  }
  process.on('SIGINT', () => handleShutdown('exit'))
  process.on('SIGTERM', () => handleShutdown('termination'))
  process.on('uncaughtException', async (error) => {
    logger.main.withError(error).error('Uncaught exception')
    await cleanup(adapters, browserCtx)
    process.exit(1)
  })
}
async function initializeApp() {
  const config = useConfigManager().getConfig()
  logger.main.log('Starting Twitter service...')
  await initBrowser(config)
  const ctx = useContext()
  const adapters = await useAdapter().initAdapters(config, ctx)
  await useTwitterAuthServices(ctx).attemptLogin()
  return {
    adapters,
    context: ctx.context,
    browser: ctx.browser,
  }
}
async function bootstrap() {
  initLogger()
  try {
    const resources = await initializeApp()
    setupShutdownHooks(resources.adapters, resources.context)
    logger.main.log('Twitter service successfully started!')
  }
  catch (error) {
    logger.main.withError(error).error('Startup failed')
    process.exit(1)
  }
  process.on('unhandledRejection', (reason) => {
    logger.main.withError(reason).error('Unhandled Promise rejection:')
  })
}
bootstrap()