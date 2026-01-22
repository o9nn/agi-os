import { createWriteStream } from 'fs'
import { join } from 'path'
import { LOGS_DIR } from './config'
import { stdout, stderr } from 'process'
stdout.on('error', () => {})
stderr.on('error', () => {})
function logName() {
  const d = new Date()
  function pad(number: number) {
    return number < 10 ? '0' + number : number
  }
  const fileName = [
    `${d.getFullYear()}-`,
    `${pad(d.getMonth() + 1)}-`,
    `${pad(d.getDate())}-`,
    `${pad(d.getHours())}-`,
    `${pad(d.getMinutes())}-`,
    `${pad(d.getSeconds())}`,
    '.log',
  ].join('')
  return join(LOGS_DIR, fileName)
}
export function createLogHandler() {
  const fileName = logName()
  const stream = createWriteStream(fileName, { flags: 'w' })
  console.log(`Logfile: ${fileName}`)
  return {
    log: ((
      channel: string,
      level: string,
      stacktrace: any[],
      ...args: any[]
    ) => {
      const timestamp = new Date().toISOString()
      let line = [timestamp, fillString(channel, 22), level]
      line = line.concat(
        [stacktrace, ...args].map(value => JSON.stringify(value))
      )
      if (stream.writable) {
        stream.write(`${line.join('\t')}\n`)
      } else {
        console.warn('tried to log something after logger shut down', {
          channel,
          level,
          args,
          stacktrace,
        })
      }
    }) as LogHandlerFunction,
    end: () => stream.end(),
    logFilePath: () => fileName,
  }
}
export type LogHandler = ReturnType<typeof createLogHandler>
import { readdir, lstat, unlink } from 'fs/promises'
import { getLogger, LogHandlerFunction } from '../../shared/logger.js'
export async function cleanupLogFolder() {
  const log = getLogger('logger/log-cleanup')
  const logDir = LOGS_DIR
  const logDirContent = await readdir(logDir)
  const filesWithDates = await Promise.all(
    logDirContent.map(async logFileName => ({
      filename: logFileName,
      mtime: (await lstat(join(logDir, logFileName))).mtime.getTime(),
    }))
  )
  const sortedFiles = filesWithDates.sort((a, b) => a.mtime - b.mtime)
  if (sortedFiles.length > 10) {
    sortedFiles.splice(sortedFiles.length - 11)
    const fileCount = await Promise.all(
      sortedFiles.map(({ filename }) => unlink(join(logDir, filename)))
    )
    log.info(`Successfuly deleted ${fileCount.length} old logfiles`)
  } else {
    log.debug('Nothing to do (not more than 10 logfiles to delete)')
  }
}
function fillString(string: string, n: number) {
  if (string.length < n) {
    return string + ' '.repeat(n - string.length)
  }
  return string
}