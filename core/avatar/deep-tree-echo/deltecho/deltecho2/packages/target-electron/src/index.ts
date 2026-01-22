console.time('init')
import { mkdirSync, Stats, watchFile } from 'fs'
import { app as rawApp, dialog, ipcMain, protocol } from 'electron'
import rc from './rc.js'
import contextMenu from './electron-context-menu.js'
import { isWindowsStorePackage } from './isAppx.js'
import { getHelpMenu } from './help_menu.js'
import { initialisePowerMonitor } from './resume_from_sleep.js'
import type { EventEmitter } from 'events'
const hostRules = 'MAP * ~NOTFOUND, EXCLUDE *.openstreetmap.org'
rawApp.commandLine.appendSwitch('host-resolver-rules', hostRules)
rawApp.commandLine.appendSwitch('host-rules', hostRules)
rawApp.commandLine.appendSwitch('disable-features', 'IsolateSandboxedIframes')
if (rc['version'] === true || rc['v'] === true) {
console.info(BuildInfo.VERSION)
process.exit()
}
if (rc['help'] === true || rc['h'] === true) {
getHelpMenu()
process.exit()
}
protocol.registerSchemesAsPrivileged([
{
scheme: 'webxdc',
privileges: {
secure: true,
allowServiceWorkers: true,
standard: true,
supportFetchAPI: true,
stream: true,
},
},
])
const app = rawApp as ExtendedAppMainProcess
app.rc = rc
if (
!process.mas &&
!app.requestSingleInstanceLock() &&
!process.env.DC_TEST_DIR
) {
console.error('Only one instance allowed. Quitting.')
app.quit()
process.exit(0)
}
import {
getConfigPath,
getLogsPath,
getAccountsPath,
getCustomThemesPath,
} from './application-constants.js'
mkdirSync(getConfigPath(), { recursive: true })
mkdirSync(getLogsPath(), { recursive: true })
mkdirSync(getCustomThemesPath(), { recursive: true })
import { cleanupLogFolder, createLogHandler } from './log-handler.js'
const logHandler = createLogHandler()
import { getLogger, setLogHandler } from '../../shared/logger.js'
const log = getLogger('main/index')
setLogHandler(logHandler.log, rc)
log.info(
`Deltachat Version ${BuildInfo.VERSION} ${BuildInfo.GIT_REF} ${BuildInfo.BUILD_TIMESTAMP}`
)
process.on('exit', logHandler.end)
process.on('uncaughtException', err => {
const error = { message: err.message, stack: err.stack }
if (log) {
log.error('uncaughtError', error)
} else {
console.error('uncaughtException', error)
}
dialog.showErrorBox(
'Error - uncaughtException',
`See the logfile (${logHandler.logFilePath()}) for details and contact the developers about this issue:\n` +
JSON.stringify(error)
)
})
import setLanguage, { getCurrentLocaleDate } from './load-translations.js'
import * as ipc from './ipc.js'
import { init as initMenu } from './menu.js'
import { DesktopSettings } from './desktop_settings.js'
import * as mainWindow from './windows/main.js'
import { ExtendedAppMainProcess } from './types.js'
import { updateTrayIcon, hideDeltaChat, showDeltaChat } from './tray.js'
import './notifications.js'
import { acceptThemeCLI } from './themes.js'
import { webxdcStartUpCleanup } from './deltachat/webxdc.js'
import {
cleanupDraftTempDir,
cleanupInternalTempDirs,
} from './cleanup_temp_dir.js'
app.ipcReady = false
app.isQuitting = false
Promise.all([
new Promise((resolve, _reject) => app.on('ready', resolve)),
DesktopSettings.load(),
isWindowsStorePackage(),
webxdcStartUpCleanup(),
])
.then(onReady)
.catch(error => {
log.critical('Fatal Error during init', error)
dialog.showErrorBox(
'Fatal Error during init',
`[Version: ${BuildInfo.VERSION} | ${platform()} | ${arch()}]]
${error}
Also make sure you are not trying to run multiple instances of deltachat.`
)
process.exit(1)
})
let ipc_shutdown_function: (() => void) | null = null
async function onReady([_appReady, _loadedState, _appx, _webxdc_cleanup]: [
any,
any,
any,
any,
]) {
acceptThemeCLI()
setLanguage(DesktopSettings.state.locale || app.getLocale())
const cwd = getAccountsPath()
log.info(`cwd ${cwd}`)
ipc_shutdown_function = await ipc.init(cwd, logHandler)
mainWindow.init({ hidden: app.rc['minimized'] })
initMenu(logHandler)
if (rc.devmode) {
mainWindow.toggleDevTools()
}
if (app.rc['translation-watch']) {
watchFile(
join(getLocaleDirectoryPath(), '/_untranslated_en.json'),
(curr: Stats, prev: Stats) => {
if (curr.mtime !== prev.mtime) {
log.info('translation-watch: File changed reloading translation data')
mainWindow.chooseLanguage(getCurrentLocaleDate().locale)
log.info('translation-watch: reloading translation data - done')
}
}
)
}
cleanupLogFolder().catch(err =>
log.error('Cleanup of old logfiles failed: ', err)
)
cleanupDraftTempDir()
cleanupInternalTempDirs()
initialisePowerMonitor()
}
;(app as EventEmitter).once('ipcReady', () => {
if (!mainWindow.window) {
throw new Error('window does not exist, this should never happen')
}
console.timeEnd('init')
if (process.env.NODE_ENV === 'test') {
mainWindow.window.maximize()
}
updateTrayIcon()
mainWindow.window.on('close', e => {
log.debug("mainWindow.window.on('close')")
if (!app.isQuitting) {
e.preventDefault()
if (app.rc['minimized'] || DesktopSettings.state.minimizeToTray) {
log.debug("mainWindow.window.on('close') Hiding main window")
hideDeltaChat()
} else {
if (process.platform === 'darwin') {
log.debug(
"mainWindow.window.on('close') We are on mac, so lets hide the main window"
)
hideDeltaChat()
} else {
log.debug("mainWindow.window.on('close') Quitting deltachat")
quit(e)
}
}
}
})
})
export function quit(e?: Electron.Event) {
if (app.isQuitting) return
app.isQuitting = true
e?.preventDefault()
log.info('Starting app shutdown process')
try {
mainWindow.window?.close()
mainWindow.window?.destroy()
} catch (error) {
log.error('failed to close window, error:', error)
}
ipc_shutdown_function && ipc_shutdown_function()
cleanupDraftTempDir()
function doQuit() {
log.info('Quitting now. Bye.')
app.quit()
}
DesktopSettings.saveImmediate().then(() => {
setTimeout(doQuit, 500)
})
setTimeout(() => {
log.error('Saving state took too long. Quitting.')
doQuit()
}, 4000)
}
app.on('activate', () => {
log.debug("app.on('activate')")
if (!mainWindow.window) {
log.warn('window not set, this is normal on startup')
return
}
if (mainWindow.window.isVisible() === false) {
log.debug("app.on('activate') showing main window")
showDeltaChat()
} else {
log.debug("app.on('activate') mainWindow is visible, no need to show it")
}
})
app.on('before-quit', e => quit(e))
app.on('window-all-closed', () => quit())
app.on('web-contents-created', (_ev, contents) => {
const is_webxdc =
contents.session.storagePath &&
contents.session.storagePath.indexOf('webxdc_') !== -1
if (is_webxdc) {
const webxdcOpenUrl = (url: string) => {
if (url.startsWith('mailto:') || url.startsWith('openpgp4fpr:')) {
open_url(url)
mainWindow.window?.show()
}
}
contents.on('will-navigate', (ev, navigationUrl) => {
if (navigationUrl.startsWith('webxdc://')) {
return
} else if (navigationUrl.startsWith('mailto:')) {
ev.preventDefault()
webxdcOpenUrl(navigationUrl)
} else {
ev.preventDefault()
}
})
contents.on('will-frame-navigate', ev => {
if (ev.url.startsWith('webxdc://')) {
return
} else if (ev.url.startsWith('mailto:')) {
ev.preventDefault()
webxdcOpenUrl(ev.url)
} else {
ev.preventDefault()
}
})
contents.setWindowOpenHandler(_details => {
webxdcOpenUrl(_details.url)
return { action: 'deny' }
})
} else {
contents.on('will-navigate', (e, navigationUrl) => {
log.warn('blocked navigation attempt to', navigationUrl)
e.preventDefault()
})
contents.setWindowOpenHandler(_details => {
return { action: 'deny' }
})
}
contents.on('will-attach-webview', (event, _webPreferences, _params) => {
event.preventDefault()
})
})
contextMenu()
import { openUrlsAndFilesFromArgv, open_url } from './open_url.js'
import { getLocaleDirectoryPath } from './getLocaleDirectory.js'
import { join } from 'path'
import { BuildInfo } from './get-build-info.js'
import { arch, platform } from 'os'
openUrlsAndFilesFromArgv(process.argv)
ipcMain.handle('restart_app', async _ev => {
app.relaunch()
app.quit()
})