import appConfig from './application-config.js'
import { dirname, join } from 'path'
import { app, screen } from 'electron'
import { fileURLToPath } from 'url'
const __dirname = dirname(fileURLToPath(import.meta.url))
const AppFilesDir = join(__dirname, '..')
export function appIcon() {
const iconFormat = process.platform === 'win32' ? '.ico' : '.png'
return `${join(htmlDistDir(), 'images', 'deltachat' + iconFormat)}`
}
export function htmlDistDir() {
return join(AppFilesDir, 'html-dist')
}
export function windowDefaults() {
let targetFile = 'main.html'
let defaultWidth = 1000
if (process.env.NODE_ENV === 'test') {
targetFile = 'test.html'
defaultWidth = 1100
}
const { height: screenHeight, width: screenWidth } =
screen.getPrimaryDisplay().workAreaSize
const headerHeight = 38
const defaultHeight = Math.min(802 + headerHeight, screenHeight)
const x = (screenWidth - defaultWidth) / 2
const y = (screenHeight - defaultHeight) / 2
return {
bounds: {
height: defaultHeight,
width: defaultWidth,
x,
y,
},
headerHeight,
minWidth: 225,
minHeight: 125,
main: targetFile,
preload: join(htmlDistDir(), 'preload.js'),
}
}
export function getConfigPath() {
return dirname(appConfig.filePath)
}
export function getLogsPath() {
return join(getConfigPath(), 'logs')
}
export function getAccountsPath() {
return join(getConfigPath(), 'accounts')
}
export function getCustomThemesPath() {
return join(getConfigPath(), 'custom-themes')
}
export function getDraftTempDir() {
return join(app.getPath('temp'), 'chat.delta.desktop-draft')
}
export const supportedURISchemes = [
'OPENPGP4FPR:',
'MAILTO:',
'DCACCOUNT:',
'DCLOGIN:',
]
const ALLOWED_RESOURCE_FOLDERS = ['images', 'node_modules', 'html-dist']
const ALLOWED_SOURCE_FOLDERS = ['src', 'scss', 'node_modules']
const ALLOWED_CONFIG_FOLDERS = ['background']
export const ALLOWED_STATIC_FOLDERS = [
...[...ALLOWED_RESOURCE_FOLDERS, ...ALLOWED_SOURCE_FOLDERS].map(folder =>
join(AppFilesDir, folder)
),
...ALLOWED_CONFIG_FOLDERS.map(folder => join(getConfigPath(), folder)),
getDraftTempDir(),
]
export const ALLOWED_ACCOUNT_FOLDERS = [
'db.sqlite-blobs' ,
'dc.db-blobs',
'stickers',
]
export const INTERNAL_TMP_DIR_NAME = 'tmp'