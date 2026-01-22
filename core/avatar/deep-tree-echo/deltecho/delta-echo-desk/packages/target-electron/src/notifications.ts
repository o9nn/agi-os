import { platform } from 'os'
import { app, Notification, nativeImage, ipcMain } from 'electron'
import * as mainWindow from './windows/main.js'
import { appIcon } from './application-constants.js'
import type { DcNotification } from '../../shared/shared-types.js'
import { getLogger } from '../../shared/logger.js'
import type { NativeImage, IpcMainInvokeEvent } from 'electron'
const log = getLogger('main/notifications')
const isMac = platform() === 'darwin'
if (Notification.isSupported()) {
ipcMain.handle('notifications.show', showNotification)
ipcMain.handle('notifications.clear', clearNotificationsForChat)
ipcMain.handle('notifications.clearAll', clearAll)
process.on('beforeExit', clearAll)
} else {
ipcMain.handle('notifications.show', () => {})
ipcMain.handle('notifications.clear', () => {})
ipcMain.handle('notifications.clearAll', () => {})
}
function createNotification(data: DcNotification): Notification {
let icon: NativeImage | undefined = data.icon
? data.icon.startsWith('data:')
? nativeImage.createFromDataURL(data.icon)
: nativeImage.createFromPath(data.icon)
: undefined
if (!icon || icon.isEmpty()) {
if (!isMac) {
icon = nativeImage.createFromPath(appIcon())
}
}
const notificationOptions: Electron.NotificationConstructorOptions = {
title: data.title,
body:
platform() === 'linux' ? filterNotificationText(data.body) : data.body,
icon,
timeoutType: 'default',
}
if (process.platform === 'win32') {
notificationOptions.closeButtonText = undefined
}
return new Notification(notificationOptions)
}
function onClickNotification(
accountId: number,
chatId: number,
msgId: number,
_ev: Electron.Event
) {
mainWindow.send('ClickOnNotification', {
accountId,
chatId,
msgId,
})
mainWindow.show()
app.focus()
mainWindow.window?.focus()
}
const notifications: {
[accountId: number]: { [chatId: number]: Notification[] }
} = {}
function showNotification(_event: IpcMainInvokeEvent, data: DcNotification) {
const { chatId, accountId } = data
log.debug(
'Creating notification:',
Object.assign({}, data, { body: undefined, title: undefined })
)
try {
const notify = createNotification(data)
notify.on('click', Event => {
onClickNotification(data.accountId, chatId, data.messageId, Event)
notifications[accountId][chatId] =
notifications[accountId]?.[chatId]?.filter(n => n !== notify) || []
notify.close()
})
notify.on('close', () => {
if (isMac) {
notifications[accountId][chatId] =
notifications[accountId]?.[chatId]?.filter(n => n !== notify) || []
}
console.log('Notification close event triggered', notify)
})
if (!notifications[accountId]) {
notifications[accountId] = {}
}
if (notifications[accountId][chatId]) {
notifications[accountId][chatId].push(notify)
} else {
notifications[accountId][chatId] = [notify]
}
notify.show()
} catch (error) {
log.warn('could not create notification:', error)
}
}
function clearNotificationsForChat(
_: unknown,
accountId: number,
chatId: number
) {
log.debug('clearNotificationsForChat', { accountId, chatId, notifications })
if (notifications[accountId]?.[chatId]) {
for (const notify of notifications[accountId]?.[chatId] || []) {
notify.close()
}
delete notifications[accountId][chatId]
}
log.debug('after cleared Notifications', { accountId, chatId, notifications })
}
function clearAll() {
for (const accountId of Object.keys(notifications)) {
if (!Number.isNaN(Number(accountId))) {
for (const chatId of Object.keys(notifications[Number(accountId)])) {
if (!Number.isNaN(Number(chatId))) {
clearNotificationsForChat(null, Number(accountId), Number(chatId))
}
}
}
}
}
function filterNotificationText(text: string) {
return (text || '')
.replace(/&/g, '&amp;')
.replace(/</g, '&lt;')
.replace(/>/g, '&gt;')
}