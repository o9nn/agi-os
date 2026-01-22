import { AutoModel, Tensor } from '@huggingface/transformers'
import type { VADConfig, VADEventCallback, VADEvents } from './types'
export class VAD {
private buffer: Float32Array
private bufferPointer: number = 0
private config: VADConfig
private eventListeners: Partial<Record<keyof VADEvents, VADEventCallback<any>[]>> = {}
private inferenceChain: Promise<any> = Promise.resolve()
private isReady: boolean = false
private isRecording: boolean = false
private model: any
private postSpeechSamples: number = 0
private prevBuffers: Float32Array[] = []
private sampleRateTensor: Tensor
private state: Tensor
constructor(userConfig: Partial<VADConfig> = {}) {
const defaultConfig: VADConfig = {
exitThreshold: 0.1,
maxBufferDuration: 30,
minSilenceDurationMs: 400,
minSpeechDurationMs: 250,
newBufferSize: 512,
sampleRate: 16000,
speechPadMs: 80,
speechThreshold: 0.3,
}
this.config = { ...defaultConfig, ...userConfig }
this.buffer = Float32Array.from(Array.from({ length: this.config.maxBufferDuration * this.config.sampleRate }))
this.state = new Tensor('float32', Float32Array.from(Array.from({ length: 2 * 1 * 128 })), [2, 1, 128])
this.sampleRateTensor = new Tensor('int64', [this.config.sampleRate], [])
}
public async initialize(): Promise<void> {
try {
this.emit('status', { message: 'Loading VAD model...', type: 'info' })
this.model = await AutoModel.from_pretrained('onnx-community/silero-vad', {
config: { model_type: 'custom' } as any,
dtype: 'fp32',
})
this.isReady = true
this.emit('status', { message: 'VAD model loaded successfully', type: 'info' })
}
catch (error) {
this.emit('status', { message: `Failed to load VAD model: ${error}`, type: 'error' })
throw error
}
}
public off<K extends keyof VADEvents>(event: K, callback: VADEventCallback<K>): void {
if (!this.eventListeners[event])
return
this.eventListeners[event] = this.eventListeners[event].filter(cb => cb !== callback)
}
public on<K extends keyof VADEvents>(event: K, callback: VADEventCallback<K>): void {
if (!this.eventListeners[event]) {
this.eventListeners[event] = []
}
this.eventListeners[event].push(callback as any)
}
public async processAudio(inputBuffer: Float32Array): Promise<void> {
if (!this.isReady) {
throw new Error('VAD model is not initialized. Call initialize() first.')
}
const wasRecording = this.isRecording
const isSpeech = await this.detectSpeech(inputBuffer)
const sampleRateMs = this.config.sampleRate / 1000
const minSilenceDurationSamples = this.config.minSilenceDurationMs * sampleRateMs
const speechPadSamples = this.config.speechPadMs * sampleRateMs
const minSpeechDurationSamples = this.config.minSpeechDurationMs * sampleRateMs
const maxPrevBuffers = Math.ceil(speechPadSamples / this.config.newBufferSize)
if (!wasRecording && !isSpeech) {
if (this.prevBuffers.length >= maxPrevBuffers) {
this.prevBuffers.shift()
}
this.prevBuffers.push(inputBuffer.slice(0))
return
}
const remaining = this.buffer.length - this.bufferPointer
if (inputBuffer.length >= remaining) {
this.buffer.set(inputBuffer.subarray(0, remaining), this.bufferPointer)
this.bufferPointer += remaining
const overflow = inputBuffer.subarray(remaining)
this.processSpeechSegment(overflow)
return
}
else {
this.buffer.set(inputBuffer, this.bufferPointer)
this.bufferPointer += inputBuffer.length
}
if (isSpeech) {
if (!this.isRecording) {
this.emit('speech-start', undefined)
this.emit('status', { message: 'Speech detected', type: 'info' })
}
this.isRecording = true
this.postSpeechSamples = 0
return
}
this.postSpeechSamples += inputBuffer.length
if (this.postSpeechSamples >= minSilenceDurationSamples) {
if (this.bufferPointer < minSpeechDurationSamples) {
this.reset()
return
}
this.processSpeechSegment()
}
}
public updateConfig(newConfig: Partial<VADConfig>): void {
this.config = { ...this.config, ...newConfig }
if (newConfig.maxBufferDuration || newConfig.sampleRate) {
this.buffer = Float32Array.from(Array.from({ length: this.config.maxBufferDuration * this.config.sampleRate }))
this.bufferPointer = 0
}
if (newConfig.sampleRate) {
this.sampleRateTensor = new Tensor('int64', [this.config.sampleRate], [])
}
}
private async detectSpeech(buffer: Float32Array): Promise<boolean> {
const input = new Tensor('float32', buffer, [1, buffer.length])
this.inferenceChain = this.inferenceChain.then(() =>
this.model({
input,
sr: this.sampleRateTensor,
state: this.state,
}),
)
const { output, stateN } = await (this.inferenceChain)
this.state = stateN
const speechProb = output.data[0]
this.emit('debug', {
data: { probability: speechProb },
message: 'VAD score',
})
return (
speechProb > this.config.speechThreshold
|| (this.isRecording && speechProb >= this.config.exitThreshold)
)
}
private emit<K extends keyof VADEvents>(event: K, data: VADEvents[K]): void {
if (!this.eventListeners[event])
return
for (const callback of this.eventListeners[event]) {
callback(data)
}
}
private processSpeechSegment(overflow?: Float32Array): void {
const sampleRateMs = this.config.sampleRate / 1000
const speechPadSamples = this.config.speechPadMs * sampleRateMs
const duration = (this.bufferPointer / this.config.sampleRate) * 1000
const overflowLength = overflow?.length ?? 0
const prevLength = this.prevBuffers.reduce((acc, b) => acc + b.length, 0)
const finalBuffer = Float32Array.from(Array.from({ length: prevLength + this.bufferPointer + speechPadSamples }))
let offset = 0
for (const prev of this.prevBuffers) {
finalBuffer.set(prev, offset)
offset += prev.length
}
finalBuffer.set(this.buffer.slice(0, this.bufferPointer + speechPadSamples), offset)
this.emit('speech-end', undefined)
this.emit('speech-ready', {
buffer: finalBuffer,
duration,
})
if (overflow) {
this.buffer.set(overflow, 0)
}
this.reset(overflowLength)
}
private reset(offset: number = 0): void {
this.buffer.fill(0, offset)
this.bufferPointer = offset
this.isRecording = false
this.postSpeechSamples = 0
this.prevBuffers = []
}
}
export const createVAD = async (config?: Partial<VADConfig>): Promise<VAD> => {
const vad = new VAD(config)
await vad.initialize()
return vad
}