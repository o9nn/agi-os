import { useCallback } from 'react'
import useChat from './useChat'
import { BackendRemote } from '../../backend-com'
import { ChatView } from '../../contexts/ChatContext'
import { getLogger } from '../../../../shared/logger'
import type { T } from '@deltachat/jsonrpc-client'
export type JumpToMessage = (params: {
  accountId: number
  msgId: number
  msgChatId?: number
  highlight?: boolean
  focus: boolean
  msgParentId?: number
  scrollIntoViewArg?: Parameters<HTMLElement['scrollIntoView']>[0]
}) => Promise<void>
export type SendMessage = (
  accountId: number,
  chatId: number,
  message: Partial<T.MessageData>
) => Promise<void>
export type ForwardMessage = (
  accountId: number,
  messageId: number,
  chatId: number
) => Promise<void>
export type DeleteMessage = (
  accountId: number,
  messageId: number
) => Promise<void>
const log = getLogger('hooks/useMessage')
const MESSAGE_DEFAULT: T.MessageData = {
  file: null,
  filename: null,
  viewtype: null,
  html: null,
  location: null,
  overrideSenderName: null,
  quotedMessageId: null,
  quotedText: null,
  text: null,
}
export default function useMessage() {
  const { chatId, setChatView, selectChat } = useChat()
  const jumpToMessage = useCallback<JumpToMessage>(
    async ({
      accountId,
      msgId,
      msgChatId,
      highlight = true,
      focus,
      msgParentId,
      scrollIntoViewArg,
    }) => {
      log.debug(`jumpToMessage with messageId: ${msgId}`)
      if (msgChatId == undefined) {
        msgChatId = (await BackendRemote.rpc.getMessage(accountId, msgId))
          .chatId
      }
      if (msgChatId !== chatId) {
        await selectChat(accountId, msgChatId)
        msgParentId = undefined
      }
      setChatView(ChatView.MessageList)
      window.__internal_jump_to_message_asap = {
        accountId,
        chatId: msgChatId,
        jumpToMessageArgs: [
          {
            msgId,
            highlight,
            focus,
            addMessageIdToStack: msgParentId,
            scrollIntoViewArg,
          },
        ],
      }
      window.__internal_check_jump_to_message?.()
    },
    [chatId, selectChat, setChatView]
  )
  const sendMessage = useCallback<SendMessage>(
    async (
      accountId: number,
      chatId: number,
      message: Partial<T.MessageData>
    ) => {
      const msgId = await BackendRemote.rpc.sendMsg(accountId, chatId, {
        ...MESSAGE_DEFAULT,
        ...message,
      })
      jumpToMessage({
        accountId,
        msgId,
        msgChatId: chatId,
        highlight: false,
        focus: false,
      })
    },
    [jumpToMessage]
  )
  const forwardMessage = useCallback<ForwardMessage>(
    async (accountId: number, messageId: number, chatId: number) => {
      await BackendRemote.rpc.forwardMessages(accountId, [messageId], chatId)
    },
    []
  )
  const deleteMessage = useCallback<DeleteMessage>(
    async (accountId: number, messageId: number) => {
      await BackendRemote.rpc.deleteMessages(accountId, [messageId])
    },
    []
  )
  return {
    jumpToMessage,
    sendMessage,
    forwardMessage,
    deleteMessage,
  }
}