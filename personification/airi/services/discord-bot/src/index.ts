import process, { env } from 'node:process'
import { Format, LogLevel, setGlobalFormat, setGlobalLogLevel, useLogg } from '@guiiai/logg'
import { DiscordAdapter } from './adapters/airi-adapter'
setGlobalFormat(Format.Pretty)
setGlobalLogLevel(LogLevel.Log)
const log = useLogg('Bot').useGlobalConfig()
async function main() {
const adapter = new DiscordAdapter({
discordToken: env.DISCORD_TOKEN || '',
airiToken: env.AIRI_TOKEN || 'abcd',
airiUrl: env.AIRI_URL || 'ws://localhost:6121/ws',
})
await adapter.start()
async function gracefulShutdown(signal: string) {
log.log(`Received ${signal}, shutting down...`)
await adapter.stop()
process.exit(0)
}
process.on('SIGINT', async () => {
await gracefulShutdown('SIGINT')
})
process.on('SIGTERM', async () => {
await gracefulShutdown('SIGTERM')
})
}
main().catch(err => log.withError(err).error('An error occurred'))