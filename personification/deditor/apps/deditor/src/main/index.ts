import http from 'node:http'
import { dirname, join } from 'node:path'
import { env } from 'node:process'
import { fileURLToPath } from 'node:url'
import { electronApp, is, optimizer } from '@electron-toolkit/utils'
import { Format, LogLevel, setGlobalFormat, setGlobalLogLevel } from '@guiiai/logg'
import { app, BrowserWindow, screen, shell } from 'electron'
import { isMacOS } from 'std-env'
import icon from '../../resources/icon.png?asset'
import { registerDatabaseDialects } from './ipc/databases/remote/'
import { registerApp, registerDialog, registerFs, registerPath, registerSafeStorage } from './ipc/electron'
setGlobalFormat(Format.Pretty)
setGlobalLogLevel(LogLevel.Log)
if (/^true$/i.test(env.DEDITOR_REMOTE_DEBUG || '')) {
  const remoteDebugPort = Number(env.DEDITOR_REMOTE_DEBUG_PORT || '9222')
  if (Number.isNaN(remoteDebugPort) || !Number.isInteger(remoteDebugPort) || remoteDebugPort < 0 || remoteDebugPort > 65535) {
    throw new Error(`Invalid remote debug port: ${env.DEDITOR_REMOTE_DEBUG_PORT}`)
  }
  app.commandLine.appendSwitch('remote-debugging-port', String(remoteDebugPort))
  app.commandLine.appendSwitch('remote-allow-origins', `http://localhost:${remoteDebugPort}`)
}
app.dock?.setIcon(icon)
function createWindow(): BrowserWindow {
  const primaryDisplay = screen.getPrimaryDisplay()
  const { width, height } = primaryDisplay.workAreaSize
  const mainWindow = new BrowserWindow({
    title: 'Deditor',
    width,
    height,
    show: false,
    icon,
    minWidth: 1024,
    minHeight: 768,
    webPreferences: {
      preload: join(dirname(fileURLToPath(import.meta.url)), '../preload/index.mjs'),
      sandbox: false,
    },
    titleBarStyle: isMacOS ? 'hidden' : undefined,
    trafficLightPosition: isMacOS ? { x: 10, y: 10 } : undefined,
  })
  mainWindow.on('ready-to-show', () => {
    mainWindow!.show()
  })
  mainWindow.webContents.setWindowOpenHandler((details) => {
    shell.openExternal(details.url)
    return { action: 'deny' }
  })
  if (import.meta.env.DEV) {
    console.debug('Running in development mode, window will not be focused automatically.')
    mainWindow.showInactive()
  }
  if (is.dev && env.ELECTRON_RENDERER_URL) {
    mainWindow.loadURL(env.ELECTRON_RENDERER_URL)
  }
  else {
    mainWindow.loadFile(join(dirname(fileURLToPath(import.meta.url)), '../renderer/index.html'))
  }
  return mainWindow
}
app.whenReady().then(() => {
  if (/^true$/i.test(env.DEDITOR_REMOTE_DEBUG || '')) {
    const remoteDebugEndpoint = `http://localhost:${env.DEDITOR_REMOTE_DEBUG_PORT || '9222'}`
    http.get(`${remoteDebugEndpoint}/json`, (res) => {
      let data = ''
      res.on('data', chunk => data += chunk)
      res.on('end', () => {
        try {
          const targets = JSON.parse(data)
          if (targets.length > 0) {
            let wsUrl = targets[0].webSocketDebuggerUrl
            if (wsUrl.startsWith('ws://')) {
              wsUrl = wsUrl.substring(5)
              console.log(`Inspect remotely: ${remoteDebugEndpoint}/devtools/inspector.html?ws=${wsUrl}`)
              shell.openExternal(`${remoteDebugEndpoint}/devtools/inspector.html?ws=${wsUrl}`)
            }
            else {
              console.warn('[Remote Debugging] Invalid WebSocket URL:', wsUrl)
            }
          }
          else {
            console.warn('[Remote Debugging] No targets found')
          }
        }
        catch (err) {
          console.error('[Remote Debugging] Failed to parse metadata from /json:', err)
        }
      })
    }).on('error', (err) => {
      console.error('[Remote Debugging] Failed to fetch metadata from /json:', err)
    })
  }
  electronApp.setAppUserModelId('ai.moeru.deditor')
  app.on('browser-window-created', (_, window) => {
    optimizer.watchWindowShortcuts(window)
  })
  const mainWindow = createWindow()
  registerFs(mainWindow, app)
  registerPath(mainWindow, app)
  registerApp(mainWindow, app)
  registerSafeStorage(mainWindow, app)
  registerDialog(mainWindow, app)
  registerDatabaseDialects(mainWindow)
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0)
      createWindow()
  })
})
app.on('window-all-closed', () => {
  if (!isMacOS) {
    app.quit()
  }
})