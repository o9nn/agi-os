import { BackendRemote } from '../backend-com'
import { debouncedUpdateBadgeCounter } from '../system-integration/badge-counter'
import { clearNotificationsForChat } from '../system-integration/notifications'
import { C, type T } from '@deltachat/jsonrpc-client'
export async function getChatInfoByEmail(
  accountId: number,
  email: string
): Promise<{
  chatId: number | null
  contactId: number | null
}> {
  const contactId = await BackendRemote.rpc.lookupContactIdByAddr(
    accountId,
    email
  )
  const chatId = contactId
    ? await BackendRemote.rpc.getChatIdByContactId(accountId, contactId)
    : null
  return {
    contactId,
    chatId,
  }
}
export async function saveLastChatId(accountId: number, chatId: number) {
  await BackendRemote.rpc.setConfig(accountId, 'ui.lastchatid', `${chatId}`)
}
export async function getLastChatId(accountId: number): Promise<number | null> {
  const chatId = await BackendRemote.rpc.getConfig(accountId, 'ui.lastchatid')
  if (typeof chatId === 'string') {
    return parseInt(chatId, 10)
  }
  return null
}
export async function muteChat(
  accountId: number,
  chatId: number,
  duration: T.MuteDuration
) {
  await BackendRemote.rpc.setChatMuteDuration(accountId, chatId, duration)
}
export async function unmuteChat(accountId: number, chatId: number) {
  await BackendRemote.rpc.setChatMuteDuration(accountId, chatId, {
    kind: 'NotMuted',
  })
}
export function markChatAsSeen(accountId: number, chatId: number) {
  BackendRemote.rpc.marknoticedChat(accountId, chatId)
  debouncedUpdateBadgeCounter()
  clearNotificationsForChat(accountId, chatId)
}
export async function createChatByContactId(
  accountId: number,
  contactId: number | null,
  email?: string
): Promise<number> {
  if (!contactId) {
    if (!email) {
      throw new Error('either contactId or email needs to be set')
    }
    contactId = await BackendRemote.rpc.createContact(accountId, email, null)
  }
  return await BackendRemote.rpc.createChatByContactId(accountId, contactId)
}
export async function areAllContactsVerified(
  accountId: number,
  contactIds: number[]
): Promise<boolean> {
  const contacts = await BackendRemote.rpc.getContactsByIds(
    accountId,
    contactIds
  )
  return !contactIds.some(contactId => {
    return !contacts[contactId].isVerified
  })
}
export async function getDeviceChatId(
  accountId: number
): Promise<number | null> {
  const chatId = await BackendRemote.rpc.getChatIdByContactId(
    accountId,
    C.DC_CONTACT_ID_DEVICE
  )
  return chatId
}