import errorStackParser from 'error-stack-parser'
import StackFrame from 'stackframe'
import { RC_Config } from './shared-types.js'
const startTime = Date.now()
export const colorize = (light: number, code: number) => (str: string) =>
  '\x1B[' + light + ';' + code + 'm' + str + '\x1b[0m'
export const blue = colorize(1, 34)
export const red = colorize(1, 31)
export const yellow = colorize(1, 33)
export const grey = colorize(0, 37)
export const green = colorize(1, 37)
export const cyan = colorize(1, 36)
const emojiFontCss =
  'font-family: Roboto, "Apple Color Emoji", NotoEmoji, "Helvetica Neue", Arial, Helvetica, NotoMono, sans-serif !important;'
export const enum LogLevelString {
  DEBUG = 'DEBUG',
  WARNING = 'WARNING',
  INFO = 'INFO',
  ERROR = 'ERROR',
  CRITICAL = 'CRITICAL',
}
const LoggerVariants = [
  {
    log: console.debug,
    level: LogLevelString.DEBUG,
    emoji: '🕸️',
    symbol: '[D]',
  },
  {
    log: console.info,
    level: LogLevelString.INFO,
    emoji: 'ℹ️',
    symbol: blue('[i]'),
  },
  {
    log: console.warn,
    level: LogLevelString.WARNING,
    emoji: '⚠️',
    symbol: yellow('[w]'),
  },
  {
    log: console.error,
    level: LogLevelString.ERROR,
    emoji: '🚨',
    symbol: red('[E]'),
  },
  {
    log: console.error,
    level: LogLevelString.CRITICAL,
    emoji: '🚨🚨',
    symbol: red('[C]'),
  },
]
export function printProcessLogLevelInfo() {
  console.info(
    `%cLogging Levels:\n${LoggerVariants.map(v => `${v.emoji} ${v.level}`).join(
      '\n'
    )}`,
    emojiFontCss
  )
  console.info(
    `# Tips and Tricks for using the search filter in the browser console:
• Use space to separate search terms
• Exclude search terms using -
• If the search term contains spaces you should escape it with ""
Examples:
🕸️          only show debug messages
-🕸️         don't show debug messages
ℹ️          only show info messages
-ℹ️         don't show info messages
👻          only show events from background accounts (not selected accounts)
-👻         don't show events from background accounts (not selected accounts)
📡          only show events
-📡         don't show any events
[JSONRPC]   only show jsonrpc messages
-[JSONRPC]  don't show jsonrpc messages
Start deltachat with --devmode (or --log-debug and --log-to-console) argument to show full log output.
If the log seems quiet, make sure the 'All levels' drop down has 'Verbose' checked.
  `
  )
}
export type LogHandlerFunction = (
  channel: string,
  level: LogLevelString,
  stacktrace: ReturnType<typeof getStackTrace>,
  ...args: any[]
) => void
let handler: LogHandlerFunction
let rc: RC_Config = {} as any
export function setLogHandler(
  LogHandler: LogHandlerFunction,
  rcObject: RC_Config
) {
  handler = LogHandler
  rc = rcObject
}
function log(
  { channel, isMainProcess }: Logger,
  level: number,
  stacktrace: ReturnType<typeof getStackTrace>,
  args: any[]
) {
  const variant = LoggerVariants[level]
  if (!handler) {
    console.log('Failed to log message - Handler not initialized yet')
    console.log(`Log Message: ${channel} ${level} ${args.join(' ')}`)
    throw Error('Failed to log message - Handler not initialized yet')
  }
  handler(channel, variant.level, stacktrace, ...args)
  if (rc['log-to-console']) {
    if (isMainProcess) {
      const beginning = `${Math.round((Date.now() - startTime) / 100) / 10}s ${
        LoggerVariants[level].symbol
      }${grey(channel)}:`
      if (!stacktrace) {
        variant.log(beginning, ...args)
      } else {
        variant.log(
          beginning,
          ...args,
          red(
            Array.isArray(stacktrace)
              ? stacktrace.map(s => `\n${s.toString()}`).join()
              : stacktrace
          )
        )
      }
    } else {
      const prefix = `%c${variant.emoji}%c${channel}`
      const prefixStyle = [emojiFontCss, 'color:blueviolet;']
      if (stacktrace) {
        variant.log(prefix, ...prefixStyle, stacktrace, ...args)
      } else {
        variant.log(prefix, ...prefixStyle, ...args)
      }
    }
  }
}
function getStackTrace(): StackFrame[] | string {
  const rawStack: StackFrame[] = errorStackParser.parse(
    new Error('Get Stacktrace')
  )
  const stack = rawStack.slice(2, rawStack.length)
  return rc['machine-readable-stacktrace']
    ? stack
    : stack.map(s => `\n${s.toString()}`).join()
}
export class Logger {
  isMainProcess = typeof window === 'undefined'
  constructor(public readonly channel: string) {
    if (channel === 'core/event') {
      this.getStackTrace = () => ''
    }
  }
  private getStackTrace(): StackFrame[] | string {
    const rawStack: StackFrame[] = errorStackParser.parse(
      new Error('Get Stacktrace')
    )
    const stack = rawStack.slice(2, rawStack.length)
    return rc['machine-readable-stacktrace']
      ? stack
      : stack.map(s => `\n${s.toString()}`).join()
  }
  debug(...args: any[]) {
    if (!rc['log-debug']) return
    log(this, 0, '', args)
  }
  info(...args: any[]) {
    log(this, 1, '', args)
  }
  warn(...args: any[]) {
    log(this, 2, this.getStackTrace(), args)
  }
  error(...args: any[]) {
    log(this, 3, this.getStackTrace(), args)
  }
  errorWithoutStackTrace(...args: any[]) {
    log(this, 3, [], args)
  }
  critical(...args: any[]) {
    log(this, 4, this.getStackTrace(), args)
  }
}
export function getLogger(channel: string) {
  return new Logger(channel)
}
if (!('toJSON' in Error.prototype))
  Object.defineProperty(Error.prototype, 'toJSON', {
    value: function () {
      const alt = {}
      Object.getOwnPropertyNames(this).forEach(function (key) {
        alt[key] = this[key]
      }, this)
      return alt
    },
    configurable: true,
    writable: true,
  })