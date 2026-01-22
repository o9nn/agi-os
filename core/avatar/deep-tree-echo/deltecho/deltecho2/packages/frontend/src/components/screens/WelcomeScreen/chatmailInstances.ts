import type { QrWithUrl } from '../../../backend/qr'
export const CHATMAIL_INSTANCES_LIST_URL = 'https://delta.chat/chatmail'
export const DEFAULT_CHATMAIL_HOSTNAME = 'nine.testrun.org'
export const DEFAULT_CHATMAIL_QR_URL = `https://${DEFAULT_CHATMAIL_HOSTNAME}/cgi-bin/newemail.py`
export const DEFAULT_INSTANCE_PRIVACY_POLICY_URL = `https://${DEFAULT_CHATMAIL_HOSTNAME}/privacy.html`
export function isDefaultInstance(value: string): boolean {
  return value.includes(DEFAULT_CHATMAIL_HOSTNAME)
}
export function isQRWithDefaultInstance(qrWithUrl?: QrWithUrl): boolean {
  if (qrWithUrl && qrWithUrl.qr.kind === 'account') {
    return isDefaultInstance(qrWithUrl.qr.domain)
  }
  return true
}