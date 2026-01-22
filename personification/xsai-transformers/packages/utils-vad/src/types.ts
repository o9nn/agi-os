export interface VADConfig {
  exitThreshold: number
  maxBufferDuration: number
  minSilenceDurationMs: number
  minSpeechDurationMs: number
  newBufferSize: number
  sampleRate: number
  speechPadMs: number
  speechThreshold: number
}
export type VADEventCallback<K extends keyof VADEvents>
  = (event: VADEvents[K]) => void
export interface VADEvents {
  'debug': { data?: any, message: string }
  'speech-end': void
  'speech-ready': { buffer: Float32Array, duration: number }
  'speech-start': void
  'status': { message: string, type: string }
}