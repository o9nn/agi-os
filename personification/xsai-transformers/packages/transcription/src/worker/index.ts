import type {
AutomaticSpeechRecognitionPipeline,
Tensor,
} from '@huggingface/transformers'
import type { PipelineOptionsFrom } from '@xsai-transformers/shared/types'
import {
full,
pipeline,
TextStreamer,
} from '@huggingface/transformers'
import { decodeBase64 } from '@moeru/std/base64'
import { merge } from '@moeru/std/merge'
import { defineInvokeHandler, defineStreamInvokeHandler, toStreamHandler } from '@unbird/eventa'
import { createContext } from '@unbird/eventa/adapters/webworkers/worker'
import { isWebGPUSupported } from 'gpuu/webgpu'
import { load, transcribe } from '../shared'
import { MessageStatus } from '../types'
const { context } = createContext()
const MAX_NEW_TOKENS = 64
let asr: AutomaticSpeechRecognitionPipeline
const base64ToFeatures = async (base64Audio: string): Promise<Float32Array> => {
const bytes = decodeBase64(base64Audio)
let samples: Int16Array
if (bytes.length % 2 !== 0) {
samples = Int16Array.from(bytes.buffer.slice(44))
}
else {
samples = Int16Array.from(bytes.buffer)
}
const audio = Float32Array.from(Array.from({ length: samples.length }))
for (let i = 0; i < samples.length; i++) {
audio[i] = samples[i] / 32768.0
}
return audio
}
defineStreamInvokeHandler(context, load, toStreamHandler(async ({ emit, payload: { modelId, options } }) => {
const device = (await isWebGPUSupported()) ? 'webgpu' : 'wasm'
const opts = merge<PipelineOptionsFrom<typeof pipeline<'automatic-speech-recognition'>>>({
device,
progress_callback: (progress) => {
emit({ data: { progress }, type: 'progress' })
},
}, options)
emit({ data: { message: `Using device: "${device}"` }, type: 'info' })
emit({ data: { message: 'Loading models...' }, type: 'info' })
asr = await pipeline('automatic-speech-recognition', modelId, opts)
if (modelId.includes('whisper-large-v3')) {
await asr.model.generate({ input_features: full([1, 128, 3000], 0.0), max_new_tokens: 1 } as Record<string, unknown>)
}
else {
await asr.model.generate({ input_features: full([1, 80, 3000], 0.0), max_new_tokens: 1 } as Record<string, unknown>)
}
emit({ data: { message: 'Ready!', status: MessageStatus.Ready }, type: 'status' })
}))
defineInvokeHandler(context, transcribe, async ({ audio, options }) => {
if (!asr) {
throw new Error('Model not loaded yet.')
}
if (!audio || audio.length === 0) {
throw new TypeError('Invalid data format for transcribe message.')
}
let language = 'en'
if (options?.language) {
language = options.language
}
const audioData = await base64ToFeatures(audio)
const streamer = new TextStreamer(asr.tokenizer, { decode_kwargs: { skip_special_tokens: true }, skip_prompt: true })
const inputs = await asr.processor(audioData)
const outputs = await asr.model.generate({ ...inputs, language, max_new_tokens: MAX_NEW_TOKENS, streamer })
const outputText = asr.tokenizer.batch_decode(outputs as Tensor, { skip_special_tokens: true })
return { text: outputText.join('') }
})