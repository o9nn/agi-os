import { utc } from '@date-fns/utc'
import { merge } from '@moeru/std'
import { isBefore } from 'date-fns'
import { customAlphabet } from 'nanoid'
import { createToken } from './token'
import { nlsWebSocketEndpointFromRegion } from './utils'
const nanoid = customAlphabet('0123456789abcdef', 32)
interface BaseEventHeader<N> {
  appkey: string
  message_id: string
  task_id: string
  namespace: 'SpeechTranscriber'
  name: N
  status?: 20000000
  status_message?: 'GATEWAY' | 'SUCCESS' | 'Success'
}
interface BaseEvent<N, P> {
  header: BaseEventHeader<N>
  payload: P
}
export interface EventStartTranscription extends BaseEvent<'StartTranscription', {
  format?: 'pcm' | 'wav' | 'opus' | 'speex' | 'amr' | 'mp3' | 'aac'
  sample_rate?: 8000 | 16000
  enable_intermediate_result?: boolean
  enable_punctuation_prediction?: boolean
  enable_inverse_text_normalization?: boolean
  customization_id?: string
  vocabulary_id?: string
  max_sentence_silence?: number
  enable_words?: boolean
  disfluency?: boolean
  speech_noise_threshold?: number
  enable_semantic_sentence_detection?: boolean
}> {}
export interface EventStopTranscription extends BaseEvent<'StopTranscription', undefined> {}
export interface EventTranscriptionStarted extends BaseEvent<'TranscriptionStarted', {
  session_id: string
}> {}
export interface EventSentenceBegin extends BaseEvent<'SentenceBegin', {
  index: number
  time: number
}> {}
export interface EventTranscriptionResultChanged extends BaseEvent<'TranscriptionResultChanged', {
  index: number
  time: number
  result: string
  words?: {
    text: string
    startTime: number
    endTime: number
  }[]
  status: number
}> {}
export interface EventSentenceEnd extends BaseEvent<'SentenceEnd', {
  index: number
  time: number
  begin_time: number
  result: string
  confidence: number
  words?: {
    text: string
    startTime: number
    endTime: number
  }[]
  status: number
  stash_result: {
    sentenceId: number
    beginTime: number
    text: string
    currentTime: number
  }
}> {}
export interface EventTranscriptionCompleted extends BaseEvent<'TranscriptionCompleted', undefined> {}
export interface ClientEvents {
  StartTranscription: EventStartTranscription['payload']
  StopTranscription: EventStopTranscription['payload']
}
export type ClientEvent = {
  [K in keyof ClientEvents]: BaseEvent<K, ClientEvents[K]>;
}[keyof ClientEvents]
export interface ServerEvents {
  TranscriptionStarted: EventTranscriptionStarted['payload']
  SentenceBegin: EventSentenceBegin['payload']
  TranscriptionResultChanged: EventTranscriptionResultChanged['payload']
  SentenceEnd: EventSentenceEnd['payload']
  TranscriptionCompleted: EventTranscriptionCompleted['payload']
}
export type ServerEvent = {
  [K in keyof ServerEvents]: BaseEvent<K, ServerEvents[K]>;
}[keyof ServerEvents]
export function createAliyunNLSSession(
  accessKeyId: string,
  accessKeySecret: string,
  appKey: string,
  options?: {
    region?:
      | 'cn-shanghai'
      | 'cn-shanghai-internal'
      | 'cn-beijing'
      | 'cn-beijing-internal'
      | 'cn-shenzhen'
      | 'cn-shenzhen-internal'
  },
) {
  const provider = createAliyunNLSProvider(accessKeyId, accessKeySecret, appKey, options)
  const providerSessionId = nanoid()
  function start(websocketConn: WebSocket, options?: {
    sessionId?: string
  } & EventStartTranscription['payload']) {
    const mergedOptions = merge({ sessionId: providerSessionId }, options)
    websocketConn.send(JSON.stringify({
      header: {
        appkey: provider.appKey,
        message_id: nanoid(),
        task_id: mergedOptions.sessionId,
        namespace: 'SpeechTranscriber',
        name: 'StartTranscription',
      },
      payload: {
        format: 'wav',
      },
    } satisfies EventStartTranscription))
  }
  function stop(websocketConn?: WebSocket, options?: {
    sessionId?: string
  }) {
    const mergedOptions = merge({ sessionId: providerSessionId }, options)
    websocketConn?.send(JSON.stringify({
      header: {
        appkey: provider.appKey,
        message_id: nanoid(),
        task_id: mergedOptions.sessionId,
        namespace: 'SpeechTranscriber',
        name: 'StopTranscription',
      },
      payload: undefined,
    } satisfies EventStopTranscription))
  }
  function onEvent(data: unknown, cb: (event: ServerEvent) => void) {
    const event = data as ServerEvent
    cb(event)
  }
  return {
    ...provider,
    sessionId: providerSessionId,
    start,
    stop,
    onEvent,
  }
}
export function createAliyunNLSProvider(
  accessKeyId: string,
  accessKeySecret: string,
  appKey: string,
  options?: {
    region?:
      | 'cn-shanghai'
      | 'cn-shanghai-internal'
      | 'cn-beijing'
      | 'cn-beijing-internal'
      | 'cn-shenzhen'
      | 'cn-shenzhen-internal'
  },
) {
  let token: string = ''
  let tokenExpiresAt: number = utc(new Date()).getTime()
  async function websocketUrl() {
    if (!token || isBefore(new Date(tokenExpiresAt), utc(new Date()))) {
      const created = await createToken(accessKeyId, accessKeySecret, { regionId: options?.region ?? 'cn-shanghai' })
      token = created.token
      tokenExpiresAt = created.expiresAt
    }
    const url = nlsWebSocketEndpointFromRegion(options?.region ?? 'cn-shanghai')
    url.searchParams.set('token', token)
    return url.toString()
  }
  return {
    websocketUrl,
    appKey,
  }
}
export type * from './base'
export { streamTranscription } from './base'