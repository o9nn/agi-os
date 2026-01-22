#!/usr/bin/env node
import { getLogger } from 'deep-tree-echo-core'
import { Orchestrator } from '../orchestrator.js'
const log = getLogger('deep-tree-echo-orchestrator/daemon')
async function main() {
log.info('Starting Deep Tree Echo Orchestrator Daemon')
log.info('Version: 1.0.0')
try {
const orchestrator = new Orchestrator()
await orchestrator.start()
log.info('Deep Tree Echo Orchestrator started successfully')
process.on('SIGINT', async () => {
log.info('Received SIGINT, shutting down gracefully...')
await orchestrator.stop()
process.exit(0)
})
process.on('SIGTERM', async () => {
log.info('Received SIGTERM, shutting down gracefully...')
await orchestrator.stop()
process.exit(0)
})
} catch (error) {
log.error('Failed to start orchestrator:', error)
process.exit(1)
}
}
main()