export interface BaseVADConfig {
  sampleRate: number
  speechThreshold: number
  exitThreshold: number
  minSilenceDurationMs: number
  speechPadMs: number
  minSpeechDurationMs: number
  maxBufferDuration: number
  newBufferSize: number
}
export interface VADEvents {
  'speech-start': void
  'speech-end': void
  'speech-ready': { buffer: Float32Array, duration: number }
  'status': { type: string, message: string }
  'debug': { message: string, data?: any }
}
export type VADEventCallback<K extends keyof VADEvents> = (event: VADEvents[K]) => void
export interface BaseVAD {
  initialize: () => Promise<void>
  processAudio: (inputBuffer: Float32Array) => Promise<void>
  on: <K extends keyof VADEvents>(event: K, callback: VADEventCallback<K>) => void
  off: <K extends keyof VADEvents>(event: K, callback: VADEventCallback<K>) => void
}
export interface VADAudioOptions {
  audioContextOptions?: AudioContextOptions
  minChunkSize?: number
  vadConfig?: Partial<BaseVADConfig>
}
export function createVADStates(vad: BaseVAD, vadAudioWorkletUrl: string, options?: VADAudioOptions) {
  let audioWorkletNode: AudioWorkletNode | null
  let mediaStream: MediaStream | null
  let sourceNode: MediaStreamAudioSourceNode | null
  let workletInitialized: boolean
  const {
    audioContextOptions = {
      sampleRate: 16000,
      latencyHint: 'interactive',
    },
  } = options || {}
  let audioContext = new AudioContext(audioContextOptions)
  async function initialize() {
    if (!audioContext || audioContext.state === 'closed') {
      audioContext = new AudioContext(audioContextOptions)
    }
    try {
      if (!workletInitialized) {
        await audioContext.audioWorklet.addModule(vadAudioWorkletUrl)
        workletInitialized = true
      }
      audioWorkletNode = new AudioWorkletNode(audioContext, 'vad-audio-worklet-processor')
      audioWorkletNode.port.onmessage = async (event) => {
        const { buffer } = event.data
        if (buffer && buffer.length > 0) {
          await vad.processAudio(new Float32Array(buffer))
        }
      }
    }
    catch (error) {
      console.error('Failed to initialize audio worklet:', error)
      throw error
    }
  }
  async function start(stream: MediaStream) {
    if (!audioContext || !audioWorkletNode) {
      throw new Error('Audio system not initialized. Call initialize() first.')
    }
    try {
      if (audioContext.state === 'suspended') {
        await audioContext.resume()
      }
      mediaStream = stream
      sourceNode = audioContext.createMediaStreamSource(mediaStream)
      sourceNode.connect(audioWorkletNode)
      const silentGain = audioContext.createGain()
      silentGain.gain.value = 0
      audioWorkletNode.connect(silentGain)
      silentGain.connect(audioContext.destination)
    }
    catch (error) {
      console.error('Failed to start microphone:', error)
      throw error
    }
  }
  function stop() {
    if (audioContext) {
      audioContext.suspend()
    }
  }
  function dispose() {
    if (sourceNode) {
      sourceNode.disconnect()
      sourceNode = null
    }
    if (audioWorkletNode) {
      audioWorkletNode.disconnect()
      audioWorkletNode = null
    }
    if (mediaStream) {
      mediaStream.getTracks().forEach(track => track.stop())
      mediaStream = null
    }
    if (audioContext && audioContext.state !== 'closed') {
      audioContext.close()
    }
    workletInitialized = false
  }
  return {
    initialize,
    start,
    stop,
    dispose,
  }
}