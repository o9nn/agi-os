import type { Logg } from '@guiiai/logg'
import path from 'node:path'
import { Format, LogLevel, setGlobalFormat, setGlobalLogLevel, useLogg } from '@guiiai/logg'
import { useConfigManager } from '../config'
let isInitialized = false
export function initLogger(): void {
  if (isInitialized) {
    return 
  }
  setGlobalLogLevel(LogLevel.Debug)
  setGlobalFormat(Format.Pretty)
  const config = useConfigManager().getConfig()
  const logLevelMap: Record<string, LogLevel> = {
    error: LogLevel.Error,
    warn: LogLevel.Warning,
    info: LogLevel.Log,
    verbose: LogLevel.Verbose,
    debug: LogLevel.Debug,
  }
  setGlobalLogLevel(logLevelMap[config.system?.logLevel] || LogLevel.Debug)
  if (config.system?.logFormat === 'pretty') {
    setGlobalFormat(Format.Pretty)
  }
  else {
    setGlobalFormat(Format.JSON)
  }
  isInitialized = true
}
export function useLogger(name?: string): Logg {
  if (name)
    return useLogg(name).useGlobalConfig()
  const stack = new Error('logger').stack
  const caller = stack?.split('\n')[2]
  const match = caller?.match(/(?:([^/]+)\/)?([^/\s]+?)(?:\.[jt]s)?:(\d+)(?::\d+)?\)?$/)
  const dirName = match?.[1] || path.basename(path.dirname(__filename))
  const fileName = match?.[2] || path.basename(__filename, '.ts')
  const lineNumber = match?.[3] || '?'
  return useLogg(`${dirName}/${fileName}:${lineNumber}`).useGlobalConfig()
}
export const logger = {
  auth: useLogger('auth-service'),
  timeline: useLogger('timeline-service'),
  browser: useLogger('browser-adapter'),
  airi: useLogger('airi-adapter'),
  mcp: useLogger('mcp-adapter'),
  parser: useLogger('parser'),
  main: useLogger('twitter-service'),
  config: useLogger('config'),
}