import { appName } from '../../../shared/constants'
import { getLogger } from '../../../shared/logger'
import { NOTIFICATION_TYPE } from '../../../shared/constants'
import { BackendRemote } from '../backend-com'
import { isImage } from '../components/attachment/Attachment'
import { runtime } from '@deltachat-desktop/runtime-interface'
import SettingsStoreInstance from '../stores/settings'
import AccountNotificationStoreInstance from '../stores/accountNotifications'
import { C, type T } from '@deltachat/jsonrpc-client'
const log = getLogger('renderer/notifications')
export function initNotifications() {
BackendRemote.on('IncomingMsg', (accountId, { chatId, msgId }) => {
log.debug('IncomingMsg', { accountId, msgId, chatId })
incomingMessageHandler(accountId, chatId, msgId, NOTIFICATION_TYPE.MESSAGE)
})
BackendRemote.on(
'IncomingWebxdcNotify',
(accountId, { msgId, text, chatId }) => {
incomingMessageHandler(
accountId,
chatId,
msgId,
NOTIFICATION_TYPE.WEBXDC_INFO,
text
)
}
)
BackendRemote.on(
'IncomingReaction',
(accountId, { contactId, chatId, msgId, reaction }) => {
log.debug('IncomingReaction', { contactId, chatId, msgId, reaction })
incomingMessageHandler(
accountId,
chatId,
msgId,
NOTIFICATION_TYPE.REACTION,
reaction,
contactId
)
}
)
BackendRemote.on('IncomingMsgBunch', accountId => {
flushNotifications(accountId)
})
}
function isMuted(accountId: number, chatId: number) {
return BackendRemote.rpc.isChatMuted(accountId, chatId)
}
type QueuedNotification = {
chatId: number
messageId: number
notificationType: NOTIFICATION_TYPE
eventText: string
contactId?: number
}
let queuedNotifications: {
[accountId: number]: QueuedNotification[]
} = {}
function incomingMessageHandler(
accountId: number,
chatId: number,
messageId: number,
notificationType: NOTIFICATION_TYPE,
eventText = '',
contactId?: number
) {
log.debug('incomingMessageHandler: ', { chatId, messageId })
if (
SettingsStoreInstance.state &&
!SettingsStoreInstance.state.desktopSettings.notifications
) {
log.debug(
'notification ignored: notifications are turned off for whole app'
)
return
}
if (AccountNotificationStoreInstance.isAccountMuted(accountId)) {
log.debug('notification ignored: notifications are turned off for account')
return
}
if (document.hasFocus() && accountId === window.__selectedAccountId) {
log.debug(
'notification ignored: window has focus and account of the notification is selected'
)
return
}
if (typeof queuedNotifications[accountId] === 'undefined') {
queuedNotifications[accountId] = []
}
queuedNotifications[accountId].push({
chatId,
messageId,
notificationType,
eventText,
contactId,
})
}
async function showNotification(
accountId: number,
chatId: number,
messageId: number,
notificationType: NOTIFICATION_TYPE,
eventText: string,
contactId?: number
) {
const tx = window.static_translate
if (!SettingsStoreInstance.state?.desktopSettings.showNotificationContent) {
runtime.showNotification({
title: appName,
body: tx('notify_new_message'),
icon: null,
chatId,
messageId,
accountId,
notificationType,
})
} else {
try {
const notificationInfo =
await BackendRemote.rpc.getMessageNotificationInfo(accountId, messageId)
let summaryPrefix = notificationInfo.summaryPrefix ?? ''
let summaryText = notificationInfo.summaryText ?? ''
const chatName = notificationInfo.chatName
const nIcon = getNotificationIcon(notificationInfo)
let icon = nIcon[0]
const iconIsAvatar = nIcon[1]
if (notificationType === NOTIFICATION_TYPE.WEBXDC_INFO) {
let message = await BackendRemote.rpc.getMessage(accountId, messageId)
if (
message.systemMessageType === 'WebxdcInfoMessage' &&
message.parentId
) {
message = await BackendRemote.rpc.getMessage(
accountId,
message.parentId
)
}
if (message.webxdcInfo) {
summaryText = eventText
summaryPrefix = `${message.webxdcInfo.name}`
if (message.webxdcInfo.icon) {
const iconName = message.webxdcInfo.icon
const iconBlob = await BackendRemote.rpc.getWebxdcBlob(
accountId,
message.id,
iconName
)
const imageExtension = iconName.split('.').pop()
icon = `data:image/${imageExtension};base64,${iconBlob}`
}
} else {
throw new Error(`no webxdcInfo in message with id ${message.id}`)
}
} else if (notificationType === NOTIFICATION_TYPE.REACTION) {
if (contactId) {
const reactionSender = await BackendRemote.rpc.getContact(
accountId,
contactId
)
summaryText = `${tx('reaction_by_other', [
reactionSender.displayName,
eventText,
summaryText,
])}`
summaryPrefix = ''
}
}
runtime.showNotification({
title: chatName,
body: summaryPrefix ? `${summaryPrefix}: ${summaryText}` : summaryText,
icon,
iconIsAvatar,
chatId,
messageId,
accountId,
notificationType,
})
} catch (error) {
log.error('failed to create notification for message: ', messageId, error)
}
}
}
async function showGroupedNotification(
accountId: number,
notifications: QueuedNotification[]
) {
const tx = window.static_translate
if (!SettingsStoreInstance.state?.desktopSettings.showNotificationContent) {
runtime.showNotification({
title: appName,
body: tx('new_messages'),
icon: null,
chatId: 0,
messageId: 0,
accountId,
notificationType: NOTIFICATION_TYPE.MESSAGE,
})
} else {
const chatIds = [...new Set(notifications.map(({ chatId }) => chatId))]
const msgCount = notifications.length
try {
if (chatIds.length === 1) {
const notificationInfo =
await BackendRemote.rpc.getMessageNotificationInfo(
accountId,
notifications[0].messageId
)
const { chatName, chatProfileImage } = notificationInfo
runtime.showNotification({
title: chatName,
body: tx('chat_n_new_messages', String(msgCount), {
quantity: msgCount,
}),
icon: chatProfileImage || null,
chatId: chatIds[0],
messageId: 0,
accountId,
notificationType: NOTIFICATION_TYPE.MESSAGE,
})
} else {
const chatCount = chatIds.length
runtime.showNotification({
title: tx('new_messages'),
body: tx('n_messages_in_m_chats', [
String(msgCount),
String(chatCount),
]),
icon: null,
chatId: 0,
messageId: 0,
accountId,
notificationType: NOTIFICATION_TYPE.MESSAGE,
})
}
} catch (error) {
log.error('failed to create grouped notification: ', notifications, error)
}
}
}
const STARTUP_LIMIT = 1
const NORMAL_LIMIT = 3
let notificationLimit = STARTUP_LIMIT
async function flushNotifications(accountId: number) {
if (typeof queuedNotifications[accountId] === 'undefined') {
queuedNotifications[accountId] = []
}
const notifications = [...queuedNotifications[accountId]]
queuedNotifications = []
const uniqueChats = [...new Set(notifications.map(n => n.chatId))]
const mutedChats = (
await Promise.all(
uniqueChats.map(id =>
isMuted(accountId, id).then(muted => ({ muted, id }))
)
)
)
.filter(e => e.muted)
.map(e => e.id)
const filteredNotifications = (
await Promise.all(
notifications.map(async notification => {
if (!mutedChats.includes(notification.chatId)) {
return notification
}
if (SettingsStoreInstance.state?.desktopSettings.isMentionsEnabled) {
const isMention = await notificationIsMention(accountId, notification)
if (isMention) {
const chat = await BackendRemote.rpc.getBasicChatInfo(
accountId,
notification.chatId
)
if (chat.chatType === C.DC_CHAT_TYPE_GROUP) {
return notification
}
}
}
return null
})
)
).filter(notification => notification !== null)
if (filteredNotifications.length > notificationLimit) {
showGroupedNotification(accountId, notifications)
} else {
for (const {
chatId,
messageId,
notificationType,
eventText,
contactId,
} of filteredNotifications) {
await showNotification(
accountId,
chatId,
messageId,
notificationType,
eventText,
contactId
)
}
}
notificationLimit = NORMAL_LIMIT
}
async function notificationIsMention(
accountId: number,
notification: QueuedNotification
) {
if (notification.notificationType === NOTIFICATION_TYPE.WEBXDC_INFO) {
log.info('mention detected: webxdc-info notification')
return true
}
if (notification.notificationType === NOTIFICATION_TYPE.REACTION) {
log.info('mention detected: reaction to own message')
return true
}
const message = await BackendRemote.rpc.getMessage(
accountId,
notification.messageId
)
if (message.quote && message.quote.kind === 'WithMessage') {
const quote = await BackendRemote.rpc.getMessage(
accountId,
message.quote.messageId
)
if (quote.sender.id === C.DC_CONTACT_ID_SELF) {
log.info('mention detected: answer to own message')
return true
}
}
log.debug('ignoring notification on muted chat')
return false
}
export function clearNotificationsForChat(accountId: number, chatId: number) {
log.debug('clearNotificationsForChat', accountId, chatId)
runtime.clearNotifications(accountId, chatId)
}
export function clearAllNotifications() {
runtime.clearAllNotifications()
}
function getNotificationIcon(
notification: T.MessageNotificationInfo
): [icon: string | null, iconIsAvatar: boolean] {
if (notification.image && isImage(notification.imageMimeType)) {
return [notification.image, false]
} else if (notification.chatProfileImage) {
return [notification.chatProfileImage, true]
} else {
return [null, false]
}
}