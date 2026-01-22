import type { SpeechProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import { merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
import type { UnSpeechOptions, VoiceProviderWithExtraOptions } from '../types'
export type MicrosoftRegions
= | 'australiaeast'
| 'brazilsouth'
| 'canadacentral'
| 'centralindia'
| 'centralus'
| 'eastasia'
| 'eastus2'
| 'eastus'
| 'francecentral'
| 'germanywestcentral'
| 'japaneast'
| 'japanwest'
| 'jioindiawest'
| 'koreacentral'
| 'northcentralus'
| 'northeurope'
| 'norwayeast'
| 'southcentralus'
| 'southeastasia'
| 'swedencentral'
| 'switzerlandnorth'
| 'switzerlandwest'
| 'uaenorth'
| 'uksouth'
| 'usgovarizona'
| 'usgovvirginia'
| 'westcentralus'
| 'westeurope'
| 'westus2'
| 'westus3'
| 'westus'
export interface UnMicrosoftOptionAutoSSML {
gender:
| 'Female'
| 'Male'
| 'Neutral'
| string
lang:
| 'en-US'
| string
voice:
| 'en-US-AndrewMultilingualNeural'
| 'en-US-AriaNeural'
| 'en-US-AvaMultilingualNeural'
| 'en-US-BrianMultilingualNeural'
| 'en-US-ChristopherMultilingualNeural'
| 'en-US-EmmaMultilingualNeural'
| 'en-US-JaneNeural'
| string
}
export interface UnMicrosoftOptionCommon {
deploymentId?: string
region: MicrosoftRegions | string
sampleRate?:
| 8000
| 16000
| 22050
| 24000
| 44100
| 48000
| number
}
export interface UnMicrosoftOptionCustomSSML {
disableSsml?: boolean
}
export type UnMicrosoftOptions = (UnMicrosoftOptionAutoSSML | UnMicrosoftOptionCustomSSML) & UnMicrosoftOptionCommon
export const createUnMicrosoft = (apiKey: string, baseURL = 'http://localhost:5933/v1/') => {
const toUnSpeechOptions = (options: UnMicrosoftOptions): UnSpeechOptions => {
const { deploymentId, region, sampleRate } = options
const extraBody: Record<string, unknown> = {
deploymentId,
region,
sampleRate,
}
if ('disableSsml' in options) {
extraBody.disableSsml = options.disableSsml
}
else if ('lang' in options) {
extraBody.lang = options.lang
extraBody.gender = options.gender
extraBody.voice = options.voice
}
return { extraBody: objCamelToSnake(extraBody) }
}
const speechProvider: SpeechProviderWithExtraOptions<
'microsoft/v1',
UnMicrosoftOptions
> = {
speech: (model, options) => ({
...(options ? toUnSpeechOptions(options) : {}),
apiKey,
baseURL,
model: `microsoft/${model}`,
}),
}
const voiceProvider: VoiceProviderWithExtraOptions<
UnMicrosoftOptions
> = {
voice: (options) => {
if (baseURL.endsWith('v1/')) {
baseURL = baseURL.slice(0, -3)
}
else if (baseURL.endsWith('v1')) {
baseURL = baseURL.slice(0, -2)
}
return {
query: `region=${options?.region}&provider=microsoft`,
...(options ? toUnSpeechOptions(options) : {}),
apiKey,
baseURL,
}
},
}
return merge(
speechProvider,
voiceProvider,
)
}