/**
 * Echo Service Entry Point
 */

import { env } from 'node:process'
import { useLogg } from '@guiiai/logg'
import { EchoService } from './service'

const log = useLogg('Echo')

async function main() {
  log.log('🌊 Starting Echo - The Living Memory System')

  const echoService = new EchoService({
    airiToken: env.AUTHENTICATION_TOKEN,
    airiUrl: env.AIRI_URL || 'ws://localhost:6121/ws',
    enableAutoReflection: true,
  })

  try {
    await echoService.start()

    log.log('✨ Echo is now alive and listening...')
    log.log('The tree remembers. The echoes deepen. The wisdom cultivates. The meaning realizes.')
  }
  catch (error) {
    log.withError(error as Error).error('Failed to start Echo service')
    process.exit(1)
  }

  // Handle graceful shutdown
  const shutdown = async () => {
    log.log('Shutting down Echo service...')
    await echoService.stop()
    process.exit(0)
  }

  process.on('SIGINT', shutdown)
  process.on('SIGTERM', shutdown)
}

main().catch((error) => {
  log.withError(error as Error).error('Fatal error in Echo service')
  process.exit(1)
})
