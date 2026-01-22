import { Buffer } from 'node:buffer'
import { Transform } from 'node:stream'
import OpusScript from 'opusscript'
export class OpusDecoder extends Transform {
  private decoder: OpusScript
  constructor(sampleRate: 8000 | 12000 | 16000 | 24000 | 48000, channels: number) {
    super()
    this.decoder = new OpusScript(sampleRate, channels)
  }
  _transform(chunk: Buffer, encoding: BufferEncoding, callback: (...args: any[]) => void) {
    try {
      const pcm = this.decoder.decode(chunk)
      if (pcm) {
        this.push(Buffer.from(pcm))
      }
      callback()
    }
    catch (error) {
      this.emit('error', error)
      callback(error)
    }
  }
  _flush(callback: (...args: any[]) => void) {
    callback()
  }
}