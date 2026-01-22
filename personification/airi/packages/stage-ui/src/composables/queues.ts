import type { Emotion } from '../constants/emotions'
import type { UseQueueReturn } from '../utils/queue'
import { sleep } from '@moeru/std'
import { invoke } from '@vueuse/core'
import { defineStore } from 'pinia'
import { ref, shallowRef } from 'vue'
import { EMOTION_VALUES } from '../constants/emotions'
import { createQueue } from '../utils/queue'
import { createControllableStream } from '../utils/stream'
import { chunkEmitter } from '../utils/tts'
export function useEmotionsMessageQueue(emotionsQueue: UseQueueReturn<Emotion>) {
function splitEmotion(content: string) {
for (const emotion of EMOTION_VALUES) {
if (!content.includes(emotion))
continue
return {
ok: true,
emotion: emotion as Emotion,
}
}
return {
ok: false,
emotion: '' as Emotion,
}
}
return createQueue<string>({
handlers: [
async (ctx) => {
if (EMOTION_VALUES.includes(ctx.data as Emotion)) {
ctx.emit('emotion', ctx.data as Emotion)
emotionsQueue.enqueue(ctx.data as Emotion)
return
}
{
const { ok, emotion } = splitEmotion(ctx.data)
if (ok) {
ctx.emit('emotion', emotion)
emotionsQueue.enqueue(emotion)
}
}
},
],
})
}
export function useDelayMessageQueue() {
function splitDelays(content: string) {
if (!(/<\|DELAY:\d+\|>/i.test(content))) {
return {
ok: false,
delay: 0,
}
}
const delayExecArray = /<\|DELAY:(\d+)\|>/i.exec(content)
const delay = delayExecArray?.[1]
if (!delay) {
return {
ok: false,
delay: 0,
}
}
const delaySeconds = Number.parseFloat(delay)
if (delaySeconds <= 0 || Number.isNaN(delaySeconds)) {
return {
ok: true,
delay: 0,
}
}
return {
ok: true,
delay: delaySeconds,
}
}
return createQueue<string>({
handlers: [
async (ctx) => {
const { ok, delay } = splitDelays(ctx.data)
if (ok) {
ctx.emit('delay', delay)
await sleep(delay * 1000)
}
},
],
})
}
export const usePipelineCharacterSpeechPlaybackQueueStore = defineStore('pipelines:character:speech', () => {
const onPlaybackStartedHooks = ref<Array<(payload: { text: string }) => Promise<void> | void>>([])
const onPlaybackFinishedHooks = ref<Array<(payload: { text: string }) => Promise<void> | void>>([])
function onPlaybackStarted(hook: (payload: { text: string }) => Promise<void> | void) {
onPlaybackStartedHooks.value.push(hook)
}
function onPlaybackFinished(hook: (payload: { text: string }) => Promise<void> | void) {
onPlaybackFinishedHooks.value.push(hook)
}
const currentAudioSource = shallowRef<AudioBufferSourceNode>()
const audioContext = shallowRef<AudioContext>()
const audioAnalyser = shallowRef<AnalyserNode>()
function connectAudioContext(context: AudioContext) {
audioContext.value = context
}
function connectAudioAnalyser(analyser: AnalyserNode) {
audioAnalyser.value = analyser
}
function clearPlaying() {
if (currentAudioSource) {
try {
currentAudioSource.value?.stop()
currentAudioSource.value?.disconnect()
}
catch {}
currentAudioSource.value = undefined
}
}
const playbackQueue = ref(invoke(() => {
return createQueue<{ audioBuffer: AudioBuffer, text: string }>({
handlers: [
(ctx) => {
return new Promise((resolve) => {
clearPlaying()
if (!audioContext.value) {
resolve()
return
}
const source = audioContext.value.createBufferSource()
source.buffer = ctx.data.audioBuffer
source.connect(audioContext.value.destination)
source.connect(audioAnalyser.value!)
for (const hook of onPlaybackStartedHooks.value) hook({ text: ctx.data.text })
currentAudioSource.value = source
source.start(0)
source.onended = () => {
for (const hook of onPlaybackFinishedHooks.value) hook({ text: ctx.data.text })
if (currentAudioSource.value === source) {
currentAudioSource.value = undefined
}
resolve()
}
})
},
],
})
}))
function clearQueue() {
playbackQueue.value.clear()
}
function clearAll() {
clearPlaying()
clearQueue()
}
return {
onPlaybackStarted,
onPlaybackFinished,
connectAudioContext,
connectAudioAnalyser,
clearPlaying,
clearQueue,
clearAll,
currentAudioSource,
playbackQueue,
}
})
export const usePipelineWorkflowTextSegmentationStore = defineStore('pipelines:workflows:text-segmentation', () => {
const onTextSegmentedHooks = ref<Array<(segment: string) => Promise<void> | void>>([])
function onTextSegmented(hook: (segment: string) => Promise<void> | void) {
onTextSegmentedHooks.value.push(hook)
}
function clearHooks() {
onTextSegmentedHooks.value = []
}
const textSegmentationQueue = ref(invoke(() => {
const textSegmentationStream = ref()
const textSegmentationStreamController = ref<ReadableStreamDefaultController<Uint8Array>>()
const encoder = new TextEncoder()
const { stream, controller } = createControllableStream<Uint8Array>()
textSegmentationStream.value = stream
textSegmentationStreamController.value = controller
chunkEmitter(stream.getReader(), async (chunk) => {
for (const hook of onTextSegmentedHooks.value) {
await hook(chunk)
}
})
return createQueue<string>({
handlers: [
async (ctx) => {
controller.enqueue(encoder.encode(ctx.data))
},
],
})
}))
return {
onTextSegmented,
clearHooks,
textSegmentationQueue,
}
})