import type { Fetch } from '@xsai/shared'
import {
  createChatProvider,
  createEmbedProvider,
  createMetadataProvider,
  createModelProvider,
  createSpeechProvider,
  createTranscriptionProvider,
  merge,
} from '@xsai-ext/shared-providers'
export interface CreateAzureOptions {
  apiKey: (() => Promise<string> | string) | string
  apiVersion?: string
  resourceName: string
}
export const createAzure = async (options: CreateAzureOptions) => {
  const headers = typeof options.apiKey === 'string'
    ? { 'api-key': options.apiKey }
    : undefined
  const baseURL = `https://${options.resourceName}.services.ai.azure.com/models/`
  const fetch: Fetch = async (input, init) => {
    if (options.apiVersion != null)
      input.searchParams.set('api-version', options.apiVersion)
    const token = `Bearer ${typeof options.apiKey === 'function' ? await options.apiKey() : options.apiKey}`
    init.headers ??= {}
    if (Array.isArray(init.headers))
      init.headers.push(['Authorization', token])
    else if (init.headers instanceof Headers)
      init.headers.append('Authorization', token)
    else
      init.headers.Authorization = token
    return globalThis.fetch(input, init)
  }
  return merge(
    createMetadataProvider('azure'),
    createChatProvider<
      | 'Cohere-command-r'
      | 'Cohere-command-r-plus'
      | 'DeepSeek-R1'
      | 'DeepSeek-R1-Distilled-NPU-Optimized'
      | 'DeepSeek-V3'
      | 'gpt-4o'
      | 'gpt-4o-mini'
      | 'Llama-3.2-11B-Vision-Instruct'
      | 'Llama-3.2-90B-Vision-Instruct'
      | 'Llama-3.3-70B-Instruct'
      | 'Mistral-large'
      | 'Mistral-small'
      | 'o1'
      | 'o1-mini'
      | 'o1-preview'
      | 'o3-mini'
      | 'Phi-3.5-mini-instruct'
      | 'Phi-3.5-MoE-instruct'
      | 'Phi-3.5-vision-instruct'
      | 'Phi-4'
      | 'Phi-4-mini-instruct'
      | 'Phi-4-multimodal-instruct'
    >({ baseURL, fetch, headers }),
    createEmbedProvider<
      | 'Cohere-embed-v3-english'
      | 'Cohere-embed-v3-multilingual'
      | 'text-embedding-3-large'
      | 'text-embedding-3-small'
      | 'text-embedding-ada-002'
    >({ baseURL, fetch, headers }),
    createSpeechProvider<
      | 'tts'
      | 'tts-hd'
    >({ baseURL, fetch, headers }),
    createTranscriptionProvider<
      | 'openai-whisper-large'
      | 'openai-whisper-large-v3'
      | 'whisper'
    >({ baseURL, fetch, headers }),
    createModelProvider({ baseURL, fetch, headers }),
  )
}