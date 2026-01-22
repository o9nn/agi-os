import { Mp3Encoder } from '@breezystack/lamejs'
interface EncoderConfig {
  sampleRate?: number
  bitRate?: number
}
class Encoder {
  private config: Required<EncoderConfig>
  private mp3Encoder: Mp3Encoder
  private maxSamples: number
  private samplesMono: Int16Array | null
  private dataBuffer: Int8Array[] = []
  constructor(config: EncoderConfig = {}) {
    this.config = {
      sampleRate: 44100,
      bitRate: 128,
      ...config,
    }
    this.mp3Encoder = new Mp3Encoder(
      1, 
      this.config.sampleRate,
      this.config.bitRate
    )
    this.maxSamples = 1152
    this.samplesMono = null
    this.clearBuffer()
  }
  clearBuffer(): void {
    this.dataBuffer = []
  }
  appendToBuffer(buffer: ArrayBuffer | Int8Array): void {
    this.dataBuffer.push(new Int8Array(buffer))
  }
  floatTo16BitPCM(input: Float32Array, output: Int16Array): void {
    for (let i = 0; i < input.length; i++) {
      const s = Math.max(-1, Math.min(1, input[i]))
      output[i] = s < 0 ? s * 0x8000 : s * 0x7fff
    }
  }
  convertBuffer(arrayBuffer: Float32Array | ArrayBuffer): Int16Array {
    const data = new Float32Array(arrayBuffer)
    const out = new Int16Array(
      arrayBuffer instanceof ArrayBuffer
        ? arrayBuffer.byteLength / 4
        : arrayBuffer.length
    )
    this.floatTo16BitPCM(data, out)
    return out
  }
  encode(arrayBuffer: Float32Array | ArrayBuffer): void {
    this.samplesMono = this.convertBuffer(arrayBuffer)
    let remaining = this.samplesMono.length
    for (let i = 0; remaining > 0; i += this.maxSamples) {
      const left = this.samplesMono.subarray(i, i + this.maxSamples)
      const mp3buffer = this.mp3Encoder.encodeBuffer(left)
      this.appendToBuffer(mp3buffer)
      remaining -= this.maxSamples
    }
  }
  finish(): Int8Array[] {
    this.appendToBuffer(this.mp3Encoder.flush())
    return this.dataBuffer
  }
}
export default Encoder