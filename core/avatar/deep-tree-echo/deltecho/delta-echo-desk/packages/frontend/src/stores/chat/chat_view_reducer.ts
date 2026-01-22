type ScrollTo =
| ScrollToMessage
| ScrollToPosition
| ScrollToLastKnownPosition
| ScrollToBottom
| null
interface ScrollToMessage {
type: 'scrollToMessage'
msgId: number
scrollIntoViewArg?: Parameters<HTMLElement['scrollIntoView']>[0]
highlight: boolean
focus: boolean
}
interface ScrollToLastKnownPosition {
type: 'scrollToLastKnownPosition'
lastKnownScrollHeight: number
lastKnownScrollTop: number
appendedOn: 'top' | 'bottom'
}
interface ScrollToPosition {
type: 'scrollToPosition'
scrollTop: number
}
interface ScrollToBottom {
type: 'scrollToBottom'
ifClose: boolean
}
export interface ChatViewState {
scrollTo: ScrollTo
lastKnownScrollHeight: number
}
export function defaultChatViewState(): ChatViewState {
return {
scrollTo: null,
lastKnownScrollHeight: -1,
}
}
export class ChatViewReducer {
static refresh(prevState: ChatViewState): ChatViewState {
const { lastKnownScrollTop } = getLastKnownScrollPosition()
return {
...prevState,
scrollTo: {
type: 'scrollToPosition',
scrollTop: lastKnownScrollTop,
},
}
}
static appendMessagesTop(prevState: ChatViewState): ChatViewState {
const { lastKnownScrollHeight, lastKnownScrollTop } =
getLastKnownScrollPosition()
return {
...prevState,
scrollTo: {
type: 'scrollToLastKnownPosition',
lastKnownScrollHeight,
lastKnownScrollTop,
appendedOn: 'top',
},
}
}
static appendMessagesBottom(prevState: ChatViewState): ChatViewState {
const { lastKnownScrollHeight, lastKnownScrollTop } =
getLastKnownScrollPosition()
return {
...prevState,
scrollTo: {
type: 'scrollToLastKnownPosition',
lastKnownScrollTop,
lastKnownScrollHeight,
appendedOn: 'bottom',
},
}
}
static fetchedIncomingMessages(prevState: ChatViewState): ChatViewState {
const {
lastKnownScrollHeight,
} = getLastKnownScrollPosition()
return {
...prevState,
scrollTo: {
type: 'scrollToBottom',
ifClose: true,
},
lastKnownScrollHeight,
}
}
static unlockScroll(prevState: ChatViewState): ChatViewState {
return {
...prevState,
scrollTo: null,
lastKnownScrollHeight: -1,
}
}
static setMessageListItems(prevState: ChatViewState): ChatViewState {
const { lastKnownScrollHeight, lastKnownScrollTop } =
getLastKnownScrollPosition()
return {
...prevState,
scrollTo: {
type: 'scrollToLastKnownPosition',
lastKnownScrollHeight,
lastKnownScrollTop,
appendedOn: 'top',
},
}
}
static selectChat(prevState: ChatViewState): ChatViewState {
return {
...prevState,
scrollTo: {
type: 'scrollToBottom',
ifClose: false,
},
}
}
static jumpToMessage(
prevState: ChatViewState,
jumpToMessageId: number,
highlight: boolean,
focus: boolean,
scrollIntoViewArg: ScrollToMessage['scrollIntoViewArg']
): ChatViewState {
return {
...prevState,
scrollTo: {
type: 'scrollToMessage',
msgId: jumpToMessageId,
highlight,
focus,
scrollIntoViewArg,
},
}
}
}
function getLastKnownScrollPosition(): {
lastKnownScrollHeight: number
lastKnownScrollTop: number
} {
const { scrollHeight, scrollTop } = document.querySelector('#message-list')
return {
lastKnownScrollHeight: scrollHeight,
lastKnownScrollTop: scrollTop,
}
}