import { Store } from './store'
import { ActionEmitter, KeybindAction } from '../keybindings'
import { C } from '@deltachat/jsonrpc-client'
import { BackendRemote, onDCEvent, Type } from '../backend-com'
import { selectedAccountId } from '../ScreenController'
import { T } from '@deltachat/jsonrpc-client'
import {
ChatViewState,
ChatViewReducer,
defaultChatViewState,
} from './chat/chat_view_reducer'
import { ChatStoreScheduler } from './chat/chat_scheduler'
import { useEffect, useMemo, useState } from 'react'
import { useDebouncedCallback } from 'use-debounce'
import { debounce } from 'debounce'
import { getLogger } from '@deltachat-desktop/shared/logger'
const log = getLogger('messagelist')
const PAGE_SIZE = 11
interface MessageListState {
messageListItems: T.MessageListItem[]
messageCache: { [msgId: number]: T.MessageLoadResult | undefined }
newestFetchedMessageListItemIndex: number
oldestFetchedMessageListItemIndex: number
viewState: ChatViewState
jumpToMessageStack: number[]
loaded: boolean
}
const defaultState = () =>
({
messageListItems: [],
messageCache: {},
newestFetchedMessageListItemIndex: -1,
oldestFetchedMessageListItemIndex: -1,
viewState: defaultChatViewState(),
jumpToMessageStack: [],
loaded: false,
}) as MessageListState
export function useMessageList(accountId: number, chatId: number) {
const store = useMemo(() => {
const store = new MessageListStore(accountId, chatId)
store.effect.loadChat()
return store
}, [accountId, chatId])
useEffect(() => {
const cleanup = [
onDCEvent(accountId, 'MsgDelivered', ({ chatId: eventChatId, msgId }) => {
if (chatId === eventChatId) {
store.reducer.setMessageState(msgId, C.DC_STATE_OUT_DELIVERED)
}
}),
onDCEvent(accountId, 'IncomingMsg', ({ chatId: eventChatId }) => {
if (chatId === eventChatId) {
store.effect.onEventIncomingMessage()
} else {
store.log.debug(
`chatId of IncomingMsg event (${chatId}) doesn't match id of selected chat (${eventChatId}). Skipping.`
)
}
}),
onDCEvent(accountId, 'MsgRead', ({ chatId: eventChatId, msgId }) => {
if (chatId === eventChatId) {
store.reducer.setMessageState(msgId, C.DC_STATE_OUT_MDN_RCVD)
}
}),
onDCEvent(accountId, 'MsgsChanged', ({ chatId: eventChatId, msgId }) => {
if (msgId === 0 && (eventChatId === 0 || eventChatId === chatId)) {
store.effect.refresh()
} else {
store.effect.onEventMessagesChanged(msgId)
}
}),
onDCEvent(
accountId,
'ReactionsChanged',
({ chatId: eventChatId, msgId }) => {
if (msgId === 0 && (eventChatId === 0 || eventChatId === chatId)) {
store.effect.refresh()
} else {
store.effect.onEventMessagesChanged(msgId)
}
}
),
onDCEvent(accountId, 'MsgFailed', ({ chatId: eventChatId, msgId }) => {
if (chatId === eventChatId) {
store.effect.onEventMessagesChanged(msgId)
}
}),
]
return () => cleanup.forEach(off => off())
}, [accountId, chatId, store])
const [state, setState] = useState(store.getState())
useEffect(() => {
setState(store.getState())
store.subscribe(setState)
return () => store.unsubscribe(setState)
}, [store])
const [fetchMoreTop] = useDebouncedCallback(
async () => {
await store.effect.fetchMoreMessagesTop()
},
30,
{ leading: true }
)
const [fetchMoreBottom] = useDebouncedCallback(
async () => {
await store.effect.fetchMoreMessagesBottom()
},
30,
{ leading: true }
)
return { store, state, fetchMoreTop, fetchMoreBottom }
}
function getView<T>(items: T[], start: number, end: number) {
return items.slice(start, end + 1)
}
class MessageListStore extends Store<MessageListState> {
scheduler = new ChatStoreScheduler()
emitter = BackendRemote.getContextEvents(this.accountId)
constructor(
private readonly accountId: number,
private readonly chatId: number
) {
super(defaultState(), 'MessageListStore')
}
get activeView() {
const start = this.state.oldestFetchedMessageListItemIndex
const end = this.state.newestFetchedMessageListItemIndex
const view = getView(this.state.messageListItems, start, end)
return view
}
reducer = {
selectedChat: (payload: Partial<MessageListState>) => {
this.setState(_ => {
this.scheduler.unlock('scroll')
const modifiedState: MessageListState = {
...defaultState(),
...payload,
loaded: true,
}
return modifiedState
}, 'selectedChat')
},
refresh: (
messageListItems: T.MessageListItem[],
messageCache: MessageListState['messageCache'],
newestFetchedMessageListItemIndex: number,
oldestFetchedMessageListItemIndex: number
) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageListItems,
messageCache,
viewState: ChatViewReducer.refresh(state.viewState),
newestFetchedMessageListItemIndex,
oldestFetchedMessageListItemIndex,
loaded: true,
}
return modifiedState
}, 'refresh')
},
modifiedChat: (payload: { id: number } & Partial<MessageListState>) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
...payload,
}
return modifiedState
}, 'modifiedChat')
},
appendMessagesTop: (payload: {
id: number
newMessageCacheItems: MessageListState['messageCache']
oldestFetchedMessageListItemIndex: number
}) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageCache: {
...state.messageCache,
...payload.newMessageCacheItems,
},
oldestFetchedMessageListItemIndex:
payload.oldestFetchedMessageListItemIndex,
viewState: ChatViewReducer.appendMessagesTop(state.viewState),
}
return modifiedState
}, 'appendMessagesTop')
},
appendMessagesBottom: (payload: {
newMessageCacheItems: MessageListState['messageCache']
newestFetchedMessageIndex: number
}) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageCache: {
...state.messageCache,
...payload.newMessageCacheItems,
},
newestFetchedMessageListItemIndex: payload.newestFetchedMessageIndex,
viewState: ChatViewReducer.appendMessagesBottom(state.viewState),
}
return modifiedState
}, 'appendMessagesBottom')
},
fetchedIncomingMessages: (payload: {
messageListItems: MessageListState['messageListItems']
newestFetchedMessageIndex: number
newMessageCacheItems: MessageListState['messageCache']
}) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageListItems: payload.messageListItems,
messageCache: {
...state.messageCache,
...payload.newMessageCacheItems,
},
newestFetchedMessageListItemIndex: payload.newestFetchedMessageIndex,
viewState: ChatViewReducer.fetchedIncomingMessages(state.viewState),
}
return modifiedState
}, 'fetchedIncomingMessages')
},
unlockScroll: () => {
this.log.debug('unlockScroll')
this.setState(state => {
const modifiedState: MessageListState = {
...state,
viewState: ChatViewReducer.unlockScroll(state.viewState),
}
setTimeout(() => this.scheduler.unlock('scroll'), 0)
return modifiedState
}, 'unlockScroll')
},
messageChanged: (message: Type.Message) => {
const messageLoadResult: Type.MessageLoadResult = {
kind: 'message',
...message,
}
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageCache: {
...state.messageCache,
[message.id]: messageLoadResult,
},
}
return modifiedState
}, 'messageChanged')
},
setMessageState: (messageId: number, messageState: number) => {
if (this.state.messageCache[messageId] == undefined) {
this.log.warn(
`setMessageState called for message ${messageId}, ` +
`state ${messageState}, but it's not loaded. ` +
"Ignoring, in hopes that we'll automatically load it later."
)
return
}
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageCache: {
...state.messageCache,
[messageId]: {
...state.messageCache[messageId],
state: messageState,
} as Type.MessageLoadResult,
},
}
return modifiedState
}, 'setMessageState')
},
setMessageListItems: (
messageListItems: MessageListState['messageListItems']
) => {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageListItems,
viewState: ChatViewReducer.setMessageListItems(state.viewState),
}
return modifiedState
}, 'setMessageIds')
},
clearJumpStack: () => {
if (this.state.jumpToMessageStack.length !== 0) {
this.setState(state => {
const modifiedState: MessageListState = {
...state,
jumpToMessageStack: [],
}
return modifiedState
}, 'clearJumpStack')
}
},
}
effect = {
loadChat: this.scheduler.lockedQueuedEffect(
'scroll',
async () => {
const startTime = performance.now()
if (
window.__internal_jump_to_message_asap?.accountId ===
this.accountId &&
window.__internal_jump_to_message_asap.chatId === this.chatId
) {
const jumpArgs =
window.__internal_jump_to_message_asap.jumpToMessageArgs
window.__internal_jump_to_message_asap = undefined
return await this.__jumpToMessage(...jumpArgs)
}
const firstUnreadMsgIdP = BackendRemote.rpc.getFirstUnreadMessageOfChat(
this.accountId,
this.chatId
)
const messageListItemsP = BackendRemote.rpc.getMessageListItems(
this.accountId,
this.chatId,
false,
true
)
const firstUnreadMsgId = await firstUnreadMsgIdP
if (firstUnreadMsgId !== null) {
const jumpToMessageP = this.__jumpToMessage({
msgId: firstUnreadMsgId,
highlight: true,
focus: false,
scrollIntoViewArg: { block: 'center' },
})
BackendRemote.rpc
.getBasicChatInfo(this.accountId, this.chatId)
.then(chat => {
ActionEmitter.emitAction(
chat.archived
? KeybindAction.ChatList_SwitchToArchiveView
: KeybindAction.ChatList_SwitchToNormalView
)
})
return await jumpToMessageP
}
let oldestFetchedMessageListItemIndex = -1
let newestFetchedMessageListItemIndex = -1
let messageCache: MessageListState['messageCache'] = {}
const messageListItems = await messageListItemsP
if (messageListItems.length !== 0) {
oldestFetchedMessageListItemIndex = Math.max(
messageListItems.length - 1 - PAGE_SIZE,
0
)
newestFetchedMessageListItemIndex = messageListItems.length - 1
messageCache =
(await loadMessages(
this.accountId,
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
).catch(err => this.log.error('loadMessages failed', err))) || {}
}
this.log.debug('loadChat took', performance.now() - startTime)
this.reducer.selectedChat({
messageCache,
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex,
viewState: ChatViewReducer.selectChat(this.state.viewState),
})
},
'selectChat'
),
jumpToMessage: this.scheduler.lockedQueuedEffect(
'scroll',
this.__jumpToMessage.bind(this),
'jumpToMessage'
),
loadMissingMessages: debounce(
this.scheduler.lockedQueuedEffect(
'scroll',
async () => {
const { messageCache } = this.state
const missing_message_ids: number[] = []
for (const item of this.activeView) {
if (item.kind === 'message' && !messageCache[item.msg_id]) {
missing_message_ids.push(item.msg_id)
}
}
if (missing_message_ids.length === 0) {
return
}
this.log.warn(
'Message store cache misses messages, trying to load them now',
missing_message_ids
)
const newMessageCacheItems = await BackendRemote.rpc.getMessages(
this.accountId,
missing_message_ids
)
this.setState(state => {
const modifiedState: MessageListState = {
...state,
messageCache: {
...state.messageCache,
...newMessageCacheItems,
},
}
return modifiedState
}, 'loadMissingMessagesAppend')
},
'loadMissingMessages'
),
400
),
fetchMoreMessagesTop: this.scheduler.queuedEffect(
this.scheduler.lockedEffect(
'scroll',
async () => {
this.log.debug(`fetchMoreMessagesTop`)
const state = this.state
const id = this.chatId
const oldestFetchedMessageListItemIndex = Math.max(
state.oldestFetchedMessageListItemIndex - PAGE_SIZE,
0
)
const lastMessageIndexOnLastPage =
state.oldestFetchedMessageListItemIndex
if (lastMessageIndexOnLastPage === 0) {
this.log.debug(
'FETCH_MORE_MESSAGES: lastMessageIndexOnLastPage is zero, returning'
)
return false
}
const fetchedMessageListItems = state.messageListItems.slice(
oldestFetchedMessageListItemIndex,
lastMessageIndexOnLastPage
)
if (fetchedMessageListItems.length === 0) {
this.log.debug(
'fetchMoreMessagesTop: fetchedMessageListItems.length is zero, returning'
)
return false
}
const newMessageCacheItems =
(await loadMessages(
this.accountId,
state.messageListItems,
oldestFetchedMessageListItemIndex,
lastMessageIndexOnLastPage - 1
).catch(err => this.log.error('loadMessages failed', err))) || {}
this.reducer.appendMessagesTop({
id,
newMessageCacheItems,
oldestFetchedMessageListItemIndex,
})
return true
},
'fetchMoreMessagesTop'
),
'fetchMoreMessagesTop'
),
fetchMoreMessagesBottom: this.scheduler.queuedEffect(
this.scheduler.lockedEffect(
'scroll',
async () => {
const state = this.state
const newestFetchedMessageListItemIndex =
state.newestFetchedMessageListItemIndex + 1
const newNewestFetchedMessageListItemIndex = Math.min(
newestFetchedMessageListItemIndex + PAGE_SIZE,
state.messageListItems.length - 1
)
if (
newestFetchedMessageListItemIndex === state.messageListItems.length
) {
return false
}
this.log.debug(`fetchMoreMessagesBottom`)
const fetchedMessageListItems = state.messageListItems.slice(
newestFetchedMessageListItemIndex,
newNewestFetchedMessageListItemIndex + 1
)
if (fetchedMessageListItems.length === 0) {
this.log.debug(
'fetchMoreMessagesBottom: fetchedMessageListItems.length is zero, returning',
JSON.stringify({
newestFetchedMessageIndex: newestFetchedMessageListItemIndex,
newNewestFetchedMessageIndex:
newNewestFetchedMessageListItemIndex,
messageIds: state.messageListItems,
})
)
return false
}
const newMessageCacheItems =
(await loadMessages(
this.accountId,
state.messageListItems,
newestFetchedMessageListItemIndex,
newNewestFetchedMessageListItemIndex
).catch(err => this.log.error('loadMessages failed', err))) || {}
this.reducer.appendMessagesBottom({
newMessageCacheItems,
newestFetchedMessageIndex: newNewestFetchedMessageListItemIndex,
})
return true
},
'fetchMoreMessagesBottom'
),
'fetchMoreMessagesBottom'
),
refresh: this.scheduler.queuedEffect(
this.scheduler.lockedEffect(
'scroll',
async () => {
const state = this.state
const messageListItems = await BackendRemote.rpc.getMessageListItems(
this.accountId,
this.chatId,
false,
true
)
let {
newestFetchedMessageListItemIndex,
oldestFetchedMessageListItemIndex,
} = state
newestFetchedMessageListItemIndex = Math.min(
newestFetchedMessageListItemIndex,
messageListItems.length - 1
)
oldestFetchedMessageListItemIndex = Math.max(
oldestFetchedMessageListItemIndex,
0
)
const messageCache =
(await loadMessages(
this.accountId,
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
).catch(err => this.log.error('loadMessages failed', err))) || {}
this.reducer.refresh(
messageListItems,
messageCache,
newestFetchedMessageListItemIndex,
oldestFetchedMessageListItemIndex
)
return true
},
'refresh'
),
'refresh'
),
onEventIncomingMessage: this.scheduler.queuedEffect(async () => {
const messageListItems = await BackendRemote.rpc.getMessageListItems(
this.accountId,
this.chatId,
false,
true
)
let indexEnd = -1
const last_item: Type.MessageListItem | undefined =
this.state.messageListItems[this.state.messageListItems.length - 1]
let indexStart =
last_item === undefined
? -1
: messageListItems.findIndex(item => {
if (last_item.kind !== item.kind) {
return false
} else {
if (item.kind === 'message') {
return item.msg_id === (last_item as any).msg_id
} else {
return item.timestamp === (last_item as any).timestamp
}
}
})
if (indexStart !== -1 && messageListItems[indexStart + 1]) {
indexStart = indexStart + 1
}
if (indexStart !== messageListItems.length - 1) {
indexEnd = messageListItems.length - 1
} else {
indexEnd = indexStart
}
if (
this.state.newestFetchedMessageListItemIndex !== -1 &&
indexStart !== this.state.newestFetchedMessageListItemIndex + 1
) {
this.log.debug(
`onEventIncomingMessage: new incoming messages cannot added to state without having a hole (indexStart: ${indexStart}, newestFetchedMessageListItemIndex ${this.state.newestFetchedMessageListItemIndex}), returning`
)
this.reducer.setMessageListItems(messageListItems)
return
}
const newMessageCacheItems =
(await loadMessages(
this.accountId,
messageListItems,
indexStart,
indexEnd
).catch(err => this.log.error('loadMessages failed', err))) || {}
this.reducer.fetchedIncomingMessages({
messageListItems,
newMessageCacheItems,
newestFetchedMessageIndex: indexEnd,
})
}, 'onEventIncomingMessage'),
onEventMessagesChanged: this.scheduler.queuedEffect(
async (messageId: number) => {
if (
messageId > C.DC_MSG_ID_LAST_SPECIAL &&
this.state.messageListItems.findIndex(
m => m.kind === 'message' && m.msg_id === messageId
) !== -1
) {
this.log.debug(
'DC_EVENT_MSGS_CHANGED',
'changed message seems to be message we already know'
)
try {
const message = await BackendRemote.rpc.getMessage(
this.accountId,
messageId
)
this.reducer.messageChanged(message)
} catch (error) {
this.log.warn('failed to fetch message with id', messageId, error)
return
}
} else {
if (
messageId > C.DC_MSG_ID_LAST_SPECIAL &&
(await BackendRemote.rpc.getMessage(this.accountId, messageId))
.state === C.DC_STATE_OUT_DRAFT
) {
return
}
this.log.debug(
'DC_EVENT_MSGS_CHANGED',
'changed message seems to be a new message, refetching messageIds'
)
const messageListItems = await BackendRemote.rpc.getMessageListItems(
this.accountId,
this.chatId,
false,
true
)
this.reducer.setMessageListItems(messageListItems)
}
},
'onEventMessagesChanged'
),
}
private async __jumpToMessage({
msgId: jumpToMessageId,
highlight = true,
focus,
addMessageIdToStack,
scrollIntoViewArg,
}: {
msgId: number | undefined
highlight?: boolean
focus: boolean
addMessageIdToStack?: undefined | number
scrollIntoViewArg?: Parameters<HTMLElement['scrollIntoView']>[0]
}) {
const startTime = performance.now()
this.log.debug('jumpToMessage with messageId: ', jumpToMessageId)
const accountId = selectedAccountId()
if (!accountId) {
throw new Error('no account set')
}
const chatIdPreset: number | undefined = this.chatId
let chatId: number | undefined = undefined
let jumpToMessageStack: number[] = []
if (jumpToMessageId === undefined) {
const jumpToMessageStackLength = this.state.jumpToMessageStack.length
if (jumpToMessageStackLength !== 0) {
jumpToMessageStack = this.state.jumpToMessageStack.slice(
0,
jumpToMessageStackLength - 1
)
jumpToMessageId =
this.state.jumpToMessageStack[jumpToMessageStackLength - 1]
chatId =
chatIdPreset ??
(await BackendRemote.rpc.getMessage(accountId, jumpToMessageId))
.chatId
} else {
chatId = chatIdPreset ?? this.chatId
jumpToMessageStack = []
highlight = false
}
} else {
const fromCache = this.state.messageCache[jumpToMessageId]
chatId =
chatIdPreset ??
(fromCache?.kind === 'message'
? fromCache
: await BackendRemote.rpc.getMessage(accountId, jumpToMessageId)
).chatId
if (addMessageIdToStack === undefined) {
jumpToMessageStack = []
} else {
const currentChatId = this.chatId || -1
if (chatId !== currentChatId) {
jumpToMessageStack = []
} else if (
this.state.jumpToMessageStack.indexOf(addMessageIdToStack) !== -1
) {
jumpToMessageStack = this.state.jumpToMessageStack
} else {
jumpToMessageStack = [
...this.state.jumpToMessageStack,
addMessageIdToStack,
]
}
}
}
const isMessageInCurrentChat =
this.accountId === accountId && this.chatId === chatId
if (!isMessageInCurrentChat) {
this.log.error(
'Tried to show messages from a different chat.\n' +
`this.accountId === ${this.accountId}, ` +
`this.chatId === ${this.chatId}, ` +
`target IDs: ${accountId}, ${chatId}. ` +
`jumpToMessageId === ${jumpToMessageId}`
)
}
let messageListItems = this.state.messageListItems
const findMessageIndex = (): number | undefined => {
if (jumpToMessageId == undefined) {
return messageListItems.length > 0
?
messageListItems.length - 1
: undefined
}
const ind = messageListItems.findIndex(
m => m.kind === 'message' && m.msg_id === jumpToMessageId
)
return ind === -1 ? undefined : ind
}
let jumpToMessageIndex = findMessageIndex()
const currentMessageListContainsTheMessage = jumpToMessageIndex != undefined
if (!isMessageInCurrentChat || !currentMessageListContainsTheMessage) {
messageListItems = await BackendRemote.rpc.getMessageListItems(
accountId,
chatId,
false,
true
)
jumpToMessageIndex = findMessageIndex()
}
let oldestFetchedMessageListItemIndex: number
let newestFetchedMessageListItemIndex: number
let newMessageCache: MessageListState['messageCache']
let newViewState: ChatViewState
if (messageListItems.length === 0) {
if (jumpToMessageId != undefined) {
this.log.error(
`Tried to jumpToMessage ${jumpToMessageId}, but messageListItems ` +
`is empty. Anyways, proceeding.`
)
}
oldestFetchedMessageListItemIndex = -1
newestFetchedMessageListItemIndex = -1
newMessageCache = {}
newViewState = ChatViewReducer.selectChat(this.state.viewState)
} else {
if (jumpToMessageIndex == undefined) {
this.log.error(
`messageListItems is not empty, but jumpToMessageIndex ` +
`is still undefined? Does msgId ${jumpToMessageId} ` +
`even belong to chat ${chatId}? Or did the message get deleted?\n` +
`Anyways, falling back to jumping to the last message.`
)
jumpToMessageIndex = messageListItems.length - 1
}
const half_page_size = Math.ceil(PAGE_SIZE / 2)
oldestFetchedMessageListItemIndex = Math.max(
jumpToMessageIndex - half_page_size,
0
)
newestFetchedMessageListItemIndex = Math.min(
jumpToMessageIndex + half_page_size,
messageListItems.length - 1
)
const countMessagesOnNewerSide =
newestFetchedMessageListItemIndex - jumpToMessageIndex
const countMessagesOnOlderSide =
jumpToMessageIndex - oldestFetchedMessageListItemIndex
if (countMessagesOnNewerSide < half_page_size) {
oldestFetchedMessageListItemIndex = Math.max(
oldestFetchedMessageListItemIndex -
(half_page_size - countMessagesOnNewerSide),
0
)
} else if (countMessagesOnOlderSide < half_page_size) {
newestFetchedMessageListItemIndex = Math.min(
newestFetchedMessageListItemIndex +
(half_page_size - countMessagesOnOlderSide),
messageListItems.length - 1
)
}
const messagesAlreadyLoaded = getView(
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
).every(item => {
if (item.kind === 'dayMarker') {
return true
}
const _kind: 'message' = item.kind
return this.state.messageCache[item.msg_id] != undefined
})
this.log.debug(
'messagesAlreadyLoaded:',
messagesAlreadyLoaded,
messagesAlreadyLoaded
? 'Using the existing cache'
: 'Resetting the messageCache'
)
if (messagesAlreadyLoaded) {
newMessageCache = this.state.messageCache
oldestFetchedMessageListItemIndex = Math.min(
this.state.oldestFetchedMessageListItemIndex,
oldestFetchedMessageListItemIndex
)
newestFetchedMessageListItemIndex = Math.max(
this.state.newestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
)
} else {
newMessageCache =
(await loadMessages(
accountId,
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
).catch(err => this.log.error('loadMessages failed', err))) || {}
}
if (jumpToMessageId == undefined) {
const item = messageListItems[jumpToMessageIndex]
if (item.kind !== 'message') {
this.log.error(
'messageListItems[jumpToMessageIndex] is not of type "message"??',
item,
messageListItems,
jumpToMessageIndex
)
throw new Error()
}
jumpToMessageId = item.msg_id
}
newViewState = ChatViewReducer.jumpToMessage(
this.state.viewState,
jumpToMessageId,
highlight,
focus,
scrollIntoViewArg
)
}
this.log.debug('jumpToMessage took', performance.now() - startTime)
this.reducer.selectedChat({
messageCache: newMessageCache,
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex,
viewState: newViewState,
jumpToMessageStack,
})
}
stateToHumanReadable(state: MessageListState): any {
return {
...state,
}
}
}
async function loadMessages(
accountId: number,
messageListItems: Type.MessageListItem[],
oldestFetchedMessageListItemIndex: number,
newestFetchedMessageListItemIndex: number
) {
const view = getView(
messageListItems,
oldestFetchedMessageListItemIndex,
newestFetchedMessageListItemIndex
)
.map(m => (m.kind === 'message' ? m.msg_id : C.DC_MSG_ID_LAST_SPECIAL))
.filter(msgId => msgId !== C.DC_MSG_ID_LAST_SPECIAL)
if (view.length > 100) {
log.error(
`loadMessages is loading too many (${view.length}) messages. ` +
'This is bad for performance.'
)
}
return await BackendRemote.rpc.getMessages(accountId, view)
}