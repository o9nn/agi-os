import { useCallback } from 'react'
import ConfirmationDialog from '../../components/dialogs/ConfirmationDialog'
import useDialog from '../dialog/useDialog'
import useTranslationFunction from '../useTranslationFunction'
import { createChatByContactId, getChatInfoByEmail } from '../../backend/chat'
import type { T } from '@deltachat/jsonrpc-client'
type ChatId = T.FullChat['id']
export type CreateChatByEmail = (
accountId: number,
email: string
) => Promise<ChatId | null>
export default function useCreateChatByEmail(): CreateChatByEmail {
const tx = useTranslationFunction()
const { openDialog } = useDialog()
const createChatByEmail = useCallback(
async (accountId: number, email: string) => {
const { chatId, contactId } = await getChatInfoByEmail(accountId, email)
if (chatId) {
return chatId
}
const continueProcess = await new Promise((resolve, _reject) => {
openDialog(ConfirmationDialog, {
message: tx('ask_start_chat_with', email),
confirmLabel: tx('ok'),
cb: resolve,
})
})
if (!continueProcess) {
return null
}
return await createChatByContactId(accountId, contactId, email)
},
[openDialog, tx]
)
return createChatByEmail
}