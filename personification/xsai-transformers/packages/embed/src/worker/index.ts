import type { FeatureExtractionPipeline } from '@huggingface/transformers'
import type { PipelineOptionsFrom } from '@xsai-transformers/shared/types'
import { pipeline } from '@huggingface/transformers'
import { merge } from '@moeru/std/merge'
import { defineInvokeHandler, defineStreamInvokeHandler, toStreamHandler } from '@unbird/eventa'
import { createContext } from '@unbird/eventa/adapters/webworkers/worker'
import { isWebGPUSupported } from 'gpuu/webgpu'
import { extract, load } from '../shared'
import { MessageStatus } from '../types'
const { context } = createContext()
let embed: FeatureExtractionPipeline
defineInvokeHandler(context, extract, async ({ options, text }) => {
  const result = await embed(text, options)
  const resultArray = result.tolist()
  const embedding: number[] = Array.from(resultArray[0] || [])
  return { data: embedding, dims: result.dims }
})
defineStreamInvokeHandler(context, load, toStreamHandler(async ({ emit, payload: { modelId, options } }) => {
  const device = (await isWebGPUSupported()) ? 'webgpu' : 'wasm'
  const opts = merge<PipelineOptionsFrom<typeof pipeline<'feature-extraction'>>>({
    device,
    progress_callback: (p) => {
      emit({ data: { progress: p }, type: 'progress' })
    },
  }, options)
  emit({ data: { message: `Using device: "${device}"` }, type: 'info' })
  emit({ data: { message: 'Loading models...' }, type: 'info' })
  embed = await pipeline('feature-extraction', modelId, opts)
  emit({ data: { message: 'Ready!', status: MessageStatus.Ready }, type: 'status' })
}))