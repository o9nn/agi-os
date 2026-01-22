import type { Config } from './types'
import fs from 'node:fs'
import path from 'node:path'
import process from 'node:process'
import { merge } from '@moeru/std'
import { logger } from '../utils/logger'
import { getDefaultConfig } from './types'
export class ConfigManager {
private config: Config
constructor(configPath?: string) {
this.config = getDefaultConfig()
if (configPath) {
this.loadFromFile(configPath)
}
}
private loadFromFile(filePath: string): void {
try {
const configFile = fs.readFileSync(filePath, 'utf8')
const fileConfig = JSON.parse(configFile)
this.config = merge(this.config, fileConfig)
logger.config.log(`Configuration loaded from ${filePath}`)
}
catch (error) {
logger.config.errorWithError(`Failed to load configuration file: ${(error as Error).message}`, error)
}
}
getConfig(): Config {
return this.config
}
updateConfig(newConfig: Partial<Config>): void {
this.config = merge(this.config, newConfig)
}
}
let configInstance: ConfigManager | null = null
export function useConfigManager(): ConfigManager {
if (configInstance) {
return configInstance
}
const configPath = process.env.CONFIG_PATH || path.join(process.cwd(), 'twitter-config.json')
configInstance = new ConfigManager(fs.existsSync(configPath) ? configPath : undefined)
return configInstance
}