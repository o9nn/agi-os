import { Buffer } from 'node:buffer'
import { DECODE_SAMPLE_RATE } from '../constants/audio'
export function getWavHeader(
audioLength: number,
sampleRate: number,
channelCount: number = 1,
bitsPerSample: number = 16,
): Buffer {
const wavHeader = Buffer.alloc(44)
wavHeader.write('RIFF', 0)
wavHeader.writeUInt32LE(36 + audioLength, 4)
wavHeader.write('WAVE', 8)
wavHeader.write('fmt ', 12)
wavHeader.writeUInt32LE(16, 16)
wavHeader.writeUInt16LE(1, 20)
wavHeader.writeUInt16LE(channelCount, 22)
wavHeader.writeUInt32LE(sampleRate, 24)
wavHeader.writeUInt32LE(
(sampleRate * bitsPerSample * channelCount) / 8,
28,
)
wavHeader.writeUInt16LE((bitsPerSample * channelCount) / 8, 32)
wavHeader.writeUInt16LE(bitsPerSample, 34)
wavHeader.write('data', 36)
wavHeader.writeUInt32LE(audioLength, 40)
return wavHeader
}
export function convertOpusToWav(pcmBuffer: Buffer): Buffer {
try {
const wavHeader = getWavHeader(
pcmBuffer.length,
DECODE_SAMPLE_RATE,
)
const wavBuffer = Buffer.concat([wavHeader, pcmBuffer])
return wavBuffer
}
catch (error) {
console.error('Error converting PCM to WAV:', error)
throw error
}
}