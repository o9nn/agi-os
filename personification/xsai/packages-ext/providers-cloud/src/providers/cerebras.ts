import { createChatProvider, createMetadataProvider, createModelProvider, merge } from '@xsai-ext/shared-providers'
export const createCerebras = (apiKey: string, baseURL = 'https://api.cerebras.ai/v1/') => merge(
  createMetadataProvider('cerebras'),
  createChatProvider<
    | 'llama3.1-8b'
    | 'llama-3.3-70b'
  >({ apiKey, baseURL }),
  createModelProvider({ apiKey, baseURL }),
)