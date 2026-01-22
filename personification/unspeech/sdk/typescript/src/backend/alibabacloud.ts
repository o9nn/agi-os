import type { SpeechProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import { merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
import type { UnSpeechOptions, VoiceProviderWithExtraOptions } from '../types'
export interface UnAlibabaCloudOptions {
  pitch?: number
  rate?: number
  sampleRate?: 8000 | 16000 | 22050 | 24000 | 44100 | 48000 | number
  volume?: number
}
export const createUnAlibabaCloud = (apiKey: string, baseURL = 'http://localhost:5933/v1/') => {
  const toUnSpeechOptions = (options: UnAlibabaCloudOptions): UnSpeechOptions => {
    const { pitch, rate, sampleRate, volume } = options
    const extraBody: Record<string, unknown> = {
      pitch,
      rate,
      sampleRate,
      volume,
    }
    Object.keys(extraBody).forEach(key => extraBody[key] === undefined && delete extraBody[key])
    return { extraBody: objCamelToSnake(extraBody) }
  }
  const speechProvider: SpeechProviderWithExtraOptions<
    'alibaba/v1',
    UnAlibabaCloudOptions
  > = {
    speech: (model, options) => ({
      ...(options ? toUnSpeechOptions(options) : {}),
      apiKey,
      baseURL,
      model: `alibaba/${model}`,
    }),
  }
  const voiceProvider: VoiceProviderWithExtraOptions<
    UnAlibabaCloudOptions
  > = {
    voice: (options) => {
      let adjustedBaseURL = baseURL
      if (adjustedBaseURL.endsWith('v1/')) {
        adjustedBaseURL = adjustedBaseURL.slice(0, -3)
      }
      else if (adjustedBaseURL.endsWith('v1')) {
        adjustedBaseURL = adjustedBaseURL.slice(0, -2)
      }
      return {
        query: 'provider=alibaba',
        ...(options ? toUnSpeechOptions(options) : {}),
        apiKey,
        baseURL: adjustedBaseURL,
      }
    },
  }
  return merge(
    speechProvider,
    voiceProvider,
  )
}