import { getMetadata } from 'meta-png'
import type { CharacterCardV3 } from './types'
const decodeBase64 = (base64: string) => {
const binString = atob(base64)
const size = binString.length
const bytes = new Uint8Array(size)
for (let i = 0; i < size; i++) {
bytes[i] = binString.charCodeAt(i)
}
return new TextDecoder().decode(bytes)
}
export const parseCharacterCardPNG = (png: Uint8Array): CharacterCardV3 | undefined => {
const ccv3 = getMetadata(png, 'ccv3')
if (ccv3 === undefined)
return
const result = JSON.parse(decodeBase64(ccv3)) as CharacterCardV3
return result
}