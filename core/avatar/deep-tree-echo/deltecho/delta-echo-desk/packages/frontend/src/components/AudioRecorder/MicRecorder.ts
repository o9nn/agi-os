import Encoder from './Encoder'
interface MicRecorderConfig {
  bitRate?: number
  startRecordingAt?: number
  deviceId?: string | null
  sampleRate?: number
}
class MicRecorder {
  public audioSignalDetected: boolean = false
  private onVolumeChange: (volume: number) => void
  private config: MicRecorderConfig
  private activeStream: MediaStream | null
  private context: AudioContext | null
  private microphone: MediaStreamAudioSourceNode | null
  private processor: ScriptProcessorNode | null
  private timerToStart?: number
  private lameEncoder: Encoder | null
  constructor(
    onVolumeChange: (volume: number) => void,
    config: MicRecorderConfig = {}
  ) {
    this.config = {
      bitRate: 128,
      startRecordingAt: 300,
      deviceId: null,
      ...config,
    }
    this.onVolumeChange = onVolumeChange
    this.activeStream = null
    this.context = null
    this.microphone = null
    this.processor = null
    this.lameEncoder = null
  }
  private addMicrophoneListener(stream: MediaStream): void {
    this.activeStream = stream
    this.timerToStart = window.setTimeout(() => {
      delete this.timerToStart
    }, this.config.startRecordingAt)
    if (!this.context) {
      throw new Error('AudioContext is not initialized.')
    }
    this.microphone = this.context.createMediaStreamSource(stream)
    this.processor = this.context.createScriptProcessor(0, 1, 1)
    this.processor.onaudioprocess = (event: AudioProcessingEvent) => {
      if (this.timerToStart) {
        return
      }
      if (this.lameEncoder) {
        this.lameEncoder.encode(event.inputBuffer.getChannelData(0))
      }
      this.calculateVolume(event.inputBuffer.getChannelData(0))
    }
    this.microphone.connect(this.processor)
    this.processor.connect(this.context.destination)
  }
  stop(): this {
    if (this.processor && this.microphone) {
      this.microphone.disconnect()
      this.processor.disconnect()
      if (this.context && this.context.state !== 'closed') {
        this.context.close()
      }
      this.processor.onaudioprocess = null
      if (this.activeStream) {
        this.activeStream.getAudioTracks().forEach(track => track.stop())
      }
    }
    return this
  }
  start(): Promise<MediaStream> {
    const AudioContext =
      window.AudioContext || (window as any).webkitAudioContext
    this.context = new AudioContext()
    this.config.sampleRate = this.context.sampleRate
    this.lameEncoder = new Encoder(this.config)
    const audio = this.config.deviceId
      ? { deviceId: { exact: this.config.deviceId } }
      : true
    return new Promise((resolve, reject) => {
      navigator.mediaDevices
        .getUserMedia({ audio })
        .then(stream => {
          this.addMicrophoneListener(stream)
          resolve(stream)
        })
        .catch(err => {
          reject(err)
        })
    })
  }
  getMp3(): Promise<[Int8Array[], Blob]> {
    if (!this.lameEncoder) {
      return Promise.reject(new Error('LAME encoder is not initialized.'))
    }
    const finalBuffer = this.lameEncoder.finish()
    return new Promise((resolve, reject) => {
      if (finalBuffer.length === 0) {
        reject(new Error('No buffer to send'))
      } else {
        resolve([finalBuffer, new Blob(finalBuffer, { type: 'audio/mp3' })])
        this.lameEncoder?.clearBuffer()
      }
    })
  }
  calculateVolume(input: any) {
    let sum = 0.0
    for (let i = 0; i < input.length; ++i) {
      sum += input[i] * input[i]
    }
    if (sum > 0) {
      this.audioSignalDetected = true
    }
    this.onVolumeChange(Math.sqrt(sum / input.length))
  }
}
export default MicRecorder