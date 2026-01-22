import type { SpeechProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import { merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
import type { UnSpeechOptions, VoiceProviderWithExtraOptions } from '../types'
export interface UnElevenLabsOptions {
  applyTextNormalization?: 'auto' | 'off' | 'on'
  languageCode?: string
  nextRequestIds?: string[]
  nextText?: string
  previousRequestIds?: string[]
  previousText?: string
  pronunciationDictionaryLocators?: {
    pronunciationDictionaryId: string
    versionId: string
  }[]
  seed?: number
  voiceSettings?: {
    similarityBoost: number
    speed?: number
    stability: number
    style?: number
    useSpeakerBoost?: boolean
  }
}
export const createUnElevenLabs = (apiKey: string, baseURL = 'http://localhost:5933/v1/') => {
  const toUnSpeechOptions = ({
    applyTextNormalization,
    languageCode,
    nextRequestIds,
    nextText,
    previousRequestIds,
    previousText,
    pronunciationDictionaryLocators,
    seed,
    voiceSettings,
  }: UnElevenLabsOptions): UnSpeechOptions => ({
    extraBody: objCamelToSnake({
      applyTextNormalization,
      languageCode,
      nextRequestIds,
      nextText,
      previousRequestIds,
      previousText,
      pronunciationDictionaryLocators: pronunciationDictionaryLocators
        ? pronunciationDictionaryLocators.map(pdl => objCamelToSnake(pdl))
        : undefined,
      seed,
      voiceSettings: objCamelToSnake(voiceSettings != null
        ? voiceSettings
        : {
            similarityBoost: 0.75,
            stability: 0.5,
          }),
    }),
  })
  const speechProvider: SpeechProviderWithExtraOptions<
    'eleven_english_sts_v2' | 'eleven_flash_v2' | 'eleven_flash_v2_5' | 'eleven_multilingual_sts_v2' | 'eleven_multilingual_v2',
    UnElevenLabsOptions
  > = {
    speech: (model, options) => ({
      ...(options ? toUnSpeechOptions(options) : {}),
      apiKey,
      baseURL,
      model: `elevenlabs/${model}`,
    }),
  }
  const voiceProvider: VoiceProviderWithExtraOptions<
    UnElevenLabsOptions
  > = {
    voice: (options) => {
      if (baseURL.endsWith('v1/')) {
        baseURL = baseURL.slice(0, -3)
      }
      else if (baseURL.endsWith('v1')) {
        baseURL = baseURL.slice(0, -2)
      }
      return {
        query: 'provider=elevenlabs',
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