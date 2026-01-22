import { useCallback } from 'react'
import MailtoDialog from '../components/dialogs/MailtoDialog'
import useAlertDialog from './dialog/useAlertDialog'
import useChat from './chat/useChat'
import useCreateChatByEmail from './chat/useCreateChatByEmail'
import useCreateDraftMessage from './chat/useCreateDraftMesssage'
import useDialog from './dialog/useDialog'
import useTranslationFunction from './useTranslationFunction'
import { getLogger } from '../../../shared/logger'
import { parseMailto } from '../../../shared/parse_mailto'
const log = getLogger('renderer/hooks/useOpenMailtoLink')
export default function useOpenMailtoLink() {
  const createChatByEmail = useCreateChatByEmail()
  const createDraftMessage = useCreateDraftMessage()
  const openAlertDialog = useAlertDialog()
  const tx = useTranslationFunction()
  const { openDialog } = useDialog()
  const { selectChat } = useChat()
  return useCallback(
    async (accountId: number, url: string, callback?: () => void) => {
      log.debug('processing mailto url:', url)
      try {
        const mailto = parseMailto(url)
        const messageText = mailto.subject
          ? mailto.subject + (mailto.body ? '\n\n' + mailto.body : '')
          : mailto.body
        if (mailto.to) {
          const chatId = await createChatByEmail(accountId, mailto.to)
          if (chatId) {
            if (messageText) {
              await createDraftMessage(accountId, chatId, messageText)
            } else {
              selectChat(accountId, chatId)
            }
          }
        } else {
          if (messageText) {
            openDialog(MailtoDialog, { messageText })
          }
        }
      } catch (error) {
        log.error('mailto decoding error', error)
        await openAlertDialog({
          message: tx('mailto_link_could_not_be_decoded', url),
        })
      }
      callback && callback()
    },
    [
      createChatByEmail,
      createDraftMessage,
      openAlertDialog,
      openDialog,
      selectChat,
      tx,
    ]
  )
}