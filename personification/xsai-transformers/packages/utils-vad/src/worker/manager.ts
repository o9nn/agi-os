import type { VADConfig } from '../types'
import type { VAD } from '../vad'
export interface VADAudioOptions {
audioContextOptions?: AudioContextOptions
minChunkSize?: number
vadConfig?: Partial<VADConfig>
}
export class VADAudioManager {
private audioContext: AudioContext | null = null
private audioWorkletNode: AudioWorkletNode | null = null
private mediaStream: MediaStream | null = null
private sourceNode: MediaStreamAudioSourceNode | null = null
private vad: VAD
private workletInitialized: boolean = false
constructor(vad: VAD, options: VADAudioOptions = {}) {
this.vad = vad
this.audioContext = new AudioContext(options.audioContextOptions || {
latencyHint: 'interactive',
sampleRate: 16000,
})
}
public dispose(): void {
this.stop()
if (this.audioContext && this.audioContext.state !== 'closed') {
this.audioContext.close()
this.audioContext = null
}
this.workletInitialized = false
}
public async initialize(workletUrl: string): Promise<void> {
if (!this.audioContext) {
throw new Error('Audio context not created')
}
try {
if (!this.workletInitialized) {
await this.audioContext.audioWorklet.addModule(workletUrl)
URL.revokeObjectURL(workletUrl)
this.workletInitialized = true
}
this.audioWorkletNode = new AudioWorkletNode(this.audioContext, 'vad-processor')
this.audioWorkletNode.port.onmessage = async (event) => {
const { buffer } = event.data
if (buffer && buffer.length > 0) {
await this.vad.processAudio(Float32Array.from(buffer))
}
}
}
catch (error) {
console.error('Failed to initialize audio worklet:', error)
throw error
}
}
public async startMicrophone(): Promise<void> {
if (!this.audioContext || !this.audioWorkletNode) {
throw new Error('Audio system not initialized. Call initialize() first.')
}
try {
if (this.audioContext.state === 'suspended') {
await this.audioContext.resume()
}
this.mediaStream = await navigator.mediaDevices.getUserMedia({
audio: {
autoGainControl: true,
echoCancellation: true,
noiseSuppression: true,
sampleRate: this.audioContext.sampleRate,
},
})
this.sourceNode = this.audioContext.createMediaStreamSource(this.mediaStream)
this.sourceNode.connect(this.audioWorkletNode)
const silentGain = this.audioContext.createGain()
silentGain.gain.value = 0
this.audioWorkletNode.connect(silentGain)
silentGain.connect(this.audioContext.destination)
}
catch (error) {
console.error('Failed to start microphone:', error)
throw error
}
}
public stop(): void {
if (this.sourceNode && this.audioWorkletNode) {
this.sourceNode.disconnect()
this.audioWorkletNode.disconnect()
}
if (this.mediaStream) {
this.mediaStream.getTracks().forEach(track => track.stop())
this.mediaStream = null
}
if (this.audioContext && this.audioContext.state !== 'closed') {
this.audioContext.suspend()
}
this.sourceNode = null
this.audioWorkletNode = null
}
public async stopMicrophone(): Promise<void> {
if (this.mediaStream) {
this.mediaStream.getTracks().forEach(track => track.stop())
this.mediaStream = null
}
if (this.sourceNode) {
this.sourceNode.disconnect()
this.sourceNode = null
}
this.audioContext?.suspend()
this.audioWorkletNode?.disconnect()
}
}