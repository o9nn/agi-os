import { userFeedback, Screens } from './ScreenController'
import '@deltachat-desktop/shared/global.d.ts'
import type { useMessageList } from './stores/messagelist'
import type { T } from '@deltachat/jsonrpc-client'
declare global {
interface Window {
exp: todo
__userFeedback: (message: userFeedback | false) => void
__changeScreen: (screen: Screens) => void
__selectAccount: (accountId: number) => Promise<void>
readonly __selectedAccountId: number | undefined
__screen: Screens
readonly __contextMenuActive: boolean
__setContextMenuActive: (newVal: boolean) => void
__settingsOpened: boolean
__keybindingsDialogOpened: boolean
__aboutDialogOpened: boolean
__setQuoteInDraft: ((msgId: number) => void) | null
__enterEditMessageMode: ((messageToEdit: T.Message) => void) | null
__reloadDraft: (() => void) | null
__chatlistSetSearch:
| ((searchTerm: string, chatId: number | null) => void)
| undefined
__refetchChatlist: undefined | (() => void)
__internal_jump_to_message_asap?: {
accountId: number
chatId: number
jumpToMessageArgs: Parameters<
ReturnType<typeof useMessageList>['store']['effect']['jumpToMessage']
>
}
__internal_check_jump_to_message?: () => void
__internal_current_message_list_instance_id?: symbol
__updateAccountListSidebar: (() => void) | undefined
}
}