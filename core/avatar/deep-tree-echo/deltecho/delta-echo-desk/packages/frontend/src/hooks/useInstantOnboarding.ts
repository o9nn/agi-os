import { useCallback, useContext } from 'react'
import useDialog from './dialog/useDialog'
import useSecureJoin from './useSecureJoin'
import { ConfigureProgressDialog } from '../components/dialogs/ConfigureProgressDialog'
import { DEFAULT_CHATMAIL_QR_URL } from '../components/screens/WelcomeScreen/chatmailInstances'
import { InstantOnboardingContext } from '../contexts/InstantOnboardingContext'
import type { T } from '@deltachat/jsonrpc-client'
import type { WelcomeQrWithUrl } from '../contexts/InstantOnboardingContext'
import type {
  AccountQr,
  LoginQr,
  VerifyContactQr,
  VerifyGroupQr,
} from '../backend/qr'
import AlertDialog from '../components/dialogs/AlertDialog'
type InstantOnboarding = {
  createInstantAccount: (accountId: number) => Promise<T.FullChat['id'] | null>
  resetInstantOnboarding: () => void
  showInstantOnboarding: boolean
  startInstantOnboardingFlow: (qrWithUrl?: WelcomeQrWithUrl) => Promise<void>
  welcomeQr?: WelcomeQrWithUrl
}
export default function useInstantOnboarding(): InstantOnboarding {
  const context = useContext(InstantOnboardingContext)
  const { openDialog } = useDialog()
  const { secureJoinContact, secureJoinGroup } = useSecureJoin()
  if (!context) {
    throw new Error(
      'useInstantOnboarding has to be used within <InstantOnboardingProvider>'
    )
  }
  const {
    setShowInstantOnboarding,
    setWelcomeQr,
    showInstantOnboarding,
    welcomeQr,
  } = context
  const startInstantOnboardingFlow = useCallback(
    async (qrWithUrl?: WelcomeQrWithUrl) => {
      setShowInstantOnboarding(true)
      setWelcomeQr(qrWithUrl)
    },
    [setWelcomeQr, setShowInstantOnboarding]
  )
  const createInstantAccount = useCallback(
    async (accountId: number): Promise<T.FullChat['id'] | null> => {
      let configurationQR = `dcaccount:${DEFAULT_CHATMAIL_QR_URL}`
      if (welcomeQr) {
        if (welcomeQr.qr.kind === 'account') {
          configurationQR = welcomeQr.url
        } else if (welcomeQr.qr.kind === 'login') {
          configurationQR = welcomeQr.url
        } else {
          const _: VerifyContactQr | VerifyGroupQr | never = welcomeQr.qr
        }
      }
      return new Promise((resolve, reject) => {
        openDialog(ConfigureProgressDialog, {
          credentials: null,
          qrCode: configurationQR,
          onSuccess: async () => {
            try {
              let chatId: number | null = null
              if (welcomeQr) {
                if (welcomeQr.qr.kind === 'askVerifyContact') {
                  chatId = await secureJoinContact(
                    accountId,
                    { ...welcomeQr, qr: welcomeQr.qr },
                    true
                  )
                } else if (welcomeQr.qr.kind === 'askVerifyGroup') {
                  chatId = await secureJoinGroup(
                    accountId,
                    { ...welcomeQr, qr: welcomeQr.qr },
                    true
                  )
                } else {
                  const _: AccountQr | LoginQr | never = welcomeQr.qr
                }
              }
              resolve(chatId)
            } catch (error: any) {
              reject(error)
            }
          },
          onFail: error => {
            openDialog(AlertDialog, { message: error })
          },
        })
      })
    },
    [openDialog, secureJoinContact, secureJoinGroup, welcomeQr]
  )
  const resetInstantOnboarding = () => {
    setWelcomeQr(undefined)
    setShowInstantOnboarding(false)
  }
  return {
    createInstantAccount,
    resetInstantOnboarding,
    showInstantOnboarding,
    startInstantOnboardingFlow,
    welcomeQr,
  }
}