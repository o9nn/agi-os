import type { SpeechProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import { merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
import type { UnSpeechOptions, VoiceProviderWithExtraOptions } from '../types'
export interface UnVolcengineOptions {
  app?: {
    appId?: string
    cluster?: 'volcano_tts' | string
  }
  audio?: {
    bitRate?: 160 | number
    contextLanguage?: 'es' | 'id' | 'pt' | string
    emotion?: 'angry' | string
    emotionScale?: number
    enableEmotion?: boolean
    encoding?: 'mp3' | 'ogg_opus' | 'pcm' | 'wav'
    explicitLanguage?: 'crosslingual' | 'en' | 'es-mx' | 'id' | 'jp' | 'pt-br' | 'zh' | string
    loudnessRatio?: number
    rate?: 8000 | 16000 | 24000 | number
    speedRatio?: number
  }
  request?: {
    cacheConfig?: Record<string, unknown>
    disableMarkdownFilter?: boolean
    enableLatexTone?: boolean
    extraParam?: string
    reqid?: string
    silenceDuration?: number
    textType?: 'ssml' | string
    useCache?: boolean
    withTimestamp?: string
  }
  user?: {
    uid?: string
  }
}
export const createUnVolcengine = (apiKey: string, baseURL = 'http://localhost:5933/v1/') => {
  const toUnSpeechOptions = (options: UnVolcengineOptions): UnSpeechOptions => {
    const extraBody: Record<string, unknown> = {
      app: {
        appid: options.app?.appId,
        token: apiKey,
      },
    }
    if (typeof options.app !== 'undefined') {
      extraBody.app = {
        ...options.app,
        appid: options.app?.appId,
        token: apiKey,
      }
    }
    if (typeof options.user !== 'undefined') {
      extraBody.user = options.user
    }
    if (typeof options.audio !== 'undefined') {
      extraBody.audio = options.audio
    }
    return { extraBody: objCamelToSnake(extraBody) }
  }
  const speechProvider: SpeechProviderWithExtraOptions<
    'volcengine/v1',
    UnVolcengineOptions
  > = {
    speech: (model, options) => ({
      ...(options ? toUnSpeechOptions(options) : {}),
      apiKey,
      baseURL,
      model: `volcengine/${model}`,
    }),
  }
  const voiceProvider: VoiceProviderWithExtraOptions<
    UnVolcengineOptions
  > = {
    voice: (options) => {
      if (baseURL.endsWith('v1/')) {
        baseURL = baseURL.slice(0, -3)
      }
      else if (baseURL.endsWith('v1')) {
        baseURL = baseURL.slice(0, -2)
      }
      return {
        query: 'provider=volcengine',
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