import { getLogger } from '../../shared/logger'
const log = getLogger('renderer/keybindings')
export enum KeybindAction {
  ChatList_SelectNextChat = 'chatlist:select-next-chat',
  ChatList_SelectPreviousChat = 'chatlist:select-previous-chat',
  ChatList_ScrollToSelectedChat = 'chatlist:scroll-to-selected-chat',
  ChatList_FocusItems = 'chatlist:focus-items',
  ChatList_FocusSearchInput = 'chatlist:focus-search',
  ChatList_SearchInChat = 'chatlist:search-in-chat',
  ChatList_ClearSearchInput = 'chatlist:clear-search',
  Composer_Focus = 'composer:focus',
  Composer_SelectReplyToUp = 'composer:select-reply-to-up',
  Composer_SelectReplyToDown = 'composer:select-reply-to-down',
  Composer_CancelReply = 'composer:cancel-reply',
  NewChat_Open = 'new-chat:open',
  Settings_Open = 'settings:open',
  KeybindingCheatSheet_Open = 'keybindinginfo:open',
  MessageList_PageUp = 'msglist:pageup',
  MessageList_PageDown = 'msglist:pagedown',
  GlobalGallery_Open = 'globalgallery:open',
  ChatList_SwitchToArchiveView = 'chatlist:switch-to-archive-view',
  ChatList_SwitchToNormalView = 'chatlist:switch-to-normal-view',
  AboutDialog_Open = 'about:open',
  ChatList_ExitSearch = 'chatlist:exit-search',
  Debug_MaybeNetwork = 'debug:maybe_network',
}
export namespace ActionEmitter {
  const handlers: Partial<{ [key in KeybindAction]: any[] }> = {}
  export function registerHandler(action: KeybindAction, handler: () => void) {
    if (!Array.isArray(handlers[action])) {
      handlers[action] = []
    }
    ;(handlers[action] as any[]).push(handler)
  }
  export function unRegisterHandler(
    action: KeybindAction,
    handler: () => void
  ) {
    if (!Array.isArray(handlers[action])) {
      return
    }
    handlers[action] = (handlers[action] as any[]).filter(h => h !== handler)
  }
  export function emitAction(action: KeybindAction) {
    if (!Array.isArray(handlers[action])) {
      return
    }
    log.debug('fire action', action, 'handlers:', handlers[action])
    ;(handlers[action] as any[]).forEach(handler => handler())
  }
}
export function keyDownEvent2Action(
  ev: KeyboardEvent
): KeybindAction | undefined {
  if (window.__contextMenuActive) {
    return
  }
  if (!ev.repeat) {
    if (ev.altKey && ev.code === 'ArrowDown') {
      return KeybindAction.ChatList_SelectNextChat
    } else if (ev.altKey && ev.code === 'ArrowUp') {
      return KeybindAction.ChatList_SelectPreviousChat
    } else if (ev.ctrlKey && ev.code === 'PageDown') {
      return KeybindAction.ChatList_SelectNextChat
    } else if (ev.ctrlKey && ev.code === 'PageUp') {
      return KeybindAction.ChatList_SelectPreviousChat
    } else if (ev.ctrlKey && ev.code === 'Tab') {
      return !ev.shiftKey
        ? KeybindAction.ChatList_SelectNextChat
        : KeybindAction.ChatList_SelectPreviousChat
    } else if (
      (ev.metaKey || ev.ctrlKey) &&
      (ev.key === 'f' || ev.code === 'KeyF')
    ) {
      if (ev.shiftKey) {
        return KeybindAction.ChatList_SearchInChat
      }
      return KeybindAction.ChatList_FocusSearchInput
    } else if (
      (ev.metaKey || ev.ctrlKey) &&
      (ev.key === 'n' || ev.code === 'KeyN')
    ) {
      return KeybindAction.NewChat_Open
    } else if (ev.ctrlKey && (ev.key === 'm' || ev.code === 'KeyM')) {
      return KeybindAction.Composer_Focus
    } else if (
      ev.code === 'ArrowUp' &&
      (ev.ctrlKey || ev.metaKey) &&
      !(ev.ctrlKey && ev.metaKey) 
    ) {
      return KeybindAction.Composer_SelectReplyToUp
    } else if (
      ev.code === 'ArrowDown' &&
      (ev.ctrlKey || ev.metaKey) &&
      !(ev.ctrlKey && ev.metaKey) 
    ) {
      return KeybindAction.Composer_SelectReplyToDown
    } else if (
      (ev.metaKey || ev.ctrlKey) &&
      (ev.key === ',' || ev.code === 'Comma')
    ) {
      return KeybindAction.Settings_Open
    } else if (ev.code === 'Escape') {
      if ((ev.target as any).id === 'chat-list-search') {
        return KeybindAction.ChatList_ExitSearch
      } else if ((ev.target as any).id === 'composer-textarea') {
        return KeybindAction.Composer_CancelReply
      }
    } else if (
      (ev.target as any).id === 'chat-list-search' &&
      (ev.key === 'Enter' || ev.code === 'ArrowDown')
    ) {
      return KeybindAction.ChatList_FocusItems
    } else if (ev.code === 'F5') {
      return KeybindAction.Debug_MaybeNetwork
    } else if (ev.code === 'PageUp') {
      if ((ev.target as HTMLElement)?.id === 'composer-textarea') {
        return KeybindAction.MessageList_PageUp
      }
    } else if (ev.code === 'PageDown') {
      if ((ev.target as HTMLElement)?.id === 'composer-textarea') {
        return KeybindAction.MessageList_PageDown
      }
    } else if (
      (ev.metaKey || ev.ctrlKey) &&
      (ev.key === '/' || ev.code === 'Slash')
    ) {
      return KeybindAction.KeybindingCheatSheet_Open
    }
  } else {
    if (ev.ctrlKey && ev.code === 'PageDown') {
      return KeybindAction.ChatList_SelectNextChat
    } else if (ev.ctrlKey && ev.code === 'PageUp') {
      return KeybindAction.ChatList_SelectPreviousChat
    } else if (ev.code === 'PageUp') {
      if ((ev.target as HTMLElement)?.id === 'composer-textarea') {
        return KeybindAction.MessageList_PageUp
      }
    } else if (ev.code === 'PageDown') {
      if ((ev.target as HTMLElement)?.id === 'composer-textarea') {
        return KeybindAction.MessageList_PageDown
      }
    } else if (ev.altKey && ev.code === 'ArrowDown') {
      return KeybindAction.ChatList_SelectNextChat
    } else if (ev.altKey && ev.code === 'ArrowUp') {
      return KeybindAction.ChatList_SelectPreviousChat
    }
  }
}
ActionEmitter.registerHandler(KeybindAction.ChatList_ExitSearch, () => {
  ActionEmitter.emitAction(KeybindAction.ChatList_ClearSearchInput)
  ActionEmitter.emitAction(KeybindAction.Composer_Focus)
})