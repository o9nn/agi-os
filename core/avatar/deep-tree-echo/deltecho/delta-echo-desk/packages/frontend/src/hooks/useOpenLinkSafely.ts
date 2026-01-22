import { useCallback } from 'react'
import useConfirmationDialog from './dialog/useConfirmationDialog'
import useOpenMailtoLink from './useOpenMailtoLink'
import useTranslationFunction from './useTranslationFunction'
import { runtime } from '@deltachat-desktop/runtime-interface'
export default function useOpenLinkSafely() {
  const openMailtoLink = useOpenMailtoLink()
  const openNonMailtoLinkSafely = useOpenNonMailtoLinkSafely()
  return useCallback(
    async (accountId: number, url: string) => {
      if (url.startsWith('mailto:')) {
        openMailtoLink(accountId, url)
      } else {
        await openNonMailtoLinkSafely(url)
      }
    },
    [openMailtoLink, openNonMailtoLinkSafely]
  )
}
export function useOpenNonMailtoLinkSafely() {
  const tx = useTranslationFunction()
  const openConfirmationDialog = useConfirmationDialog()
  return useCallback(
    async (url: string) => {
      if (
        url.toLowerCase().startsWith('http:') ||
        url.toLowerCase().startsWith('https:')
      ) {
        runtime.openLink(url)
      } else {
        const userConfirmed = await openConfirmationDialog({
          message: tx('ask_copy_unopenable_link_to_clipboard', url),
          confirmLabel: tx('menu_copy_link_to_clipboard'),
          cancelLabel: tx('no'),
        })
        if (userConfirmed) {
          runtime.writeClipboardText(url)
        }
      }
    },
    [openConfirmationDialog, tx]
  )
}