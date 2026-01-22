import { BackendRemote, Type } from '../backend-com'
import { runtime } from '@deltachat-desktop/runtime-interface'
export function initWebxdc() {
BackendRemote.on('WebxdcStatusUpdate', (accountId, { msgId }) => {
runtime.notifyWebxdcStatusUpdate(accountId, msgId)
})
BackendRemote.on('WebxdcRealtimeData', (accountId, { msgId, data }) => {
runtime.notifyWebxdcRealtimeData(accountId, msgId, data)
})
BackendRemote.on('MsgsChanged', (accountId, { msgId }) => {
runtime.notifyWebxdcMessageChanged(accountId, msgId)
})
BackendRemote.on('WebxdcInstanceDeleted', (accountId, { msgId }) => {
runtime.notifyWebxdcInstanceDeleted(accountId, msgId)
})
}
export async function internalOpenWebxdc(
accountId: number,
message: Type.Message
) {
let href = ''
let messageId = message.id
if (message.systemMessageType === 'WebxdcInfoMessage' && message.parentId) {
href = message.webxdcHref ?? ''
messageId = message.parentId
message = await BackendRemote.rpc.getMessage(accountId, messageId)
}
if (!message.webxdcInfo) {
throw new Error('no webxdc info for message ' + message)
}
const chatName = (
await BackendRemote.rpc.getBasicChatInfo(accountId, message.chatId)
).name
const account: Type.Account =
await BackendRemote.rpc.getAccountInfo(accountId)
const displayname =
account.kind === 'Configured' ? account.displayName || account.addr : null
runtime.openWebxdc(messageId, {
accountId,
displayname,
chatName,
webxdcInfo: message.webxdcInfo,
href,
})
}
export async function openMapWebxdc(accountId: number, chatId?: number) {
runtime.openMapsWebxdc(accountId, chatId)
}