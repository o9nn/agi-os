import { C } from '@deltachat/jsonrpc-client'
export function getDirection({ fromId }: { fromId: number }) {
return fromId === C.DC_CONTACT_ID_SELF ? 'outgoing' : 'incoming'
}