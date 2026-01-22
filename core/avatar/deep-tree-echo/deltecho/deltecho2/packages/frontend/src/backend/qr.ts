import { BackendRemote } from '../backend-com'
import type { T } from '@deltachat/jsonrpc-client'
export type AccountQr = Extract<T.Qr, { kind: 'account' }>
export type VerifyContactQr = Extract<T.Qr, { kind: 'askVerifyContact' }>
export type VerifyGroupQr = Extract<T.Qr, { kind: 'askVerifyGroup' }>
export type LoginQr = Extract<T.Qr, { kind: 'login' }>
export type QrWithUrl<Q = T.Qr> = {
qr: Q
url: string
}
export async function processQr(
accountId: number,
url: string
): Promise<QrWithUrl> {
const qr = await BackendRemote.rpc.checkQr(accountId, url)
if (!qr) {
throw new Error('Could not parse string')
}
return {
qr,
url,
}
}