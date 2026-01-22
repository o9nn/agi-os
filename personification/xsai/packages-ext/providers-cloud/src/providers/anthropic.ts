import {
  createChatProvider,
  createMetadataProvider,
  createModelProvider,
  merge,
} from '@xsai-ext/shared-providers'
export const createAnthropic = (apiKey: string, baseURL = 'https://api.anthropic.com/v1/') => merge(
  createMetadataProvider('anthropic'),
  createChatProvider<
    | 'claude-3-5-haiku-latest'
    | 'claude-3-5-sonnet-latest'
    | 'claude-3-7-sonnet-latest'
    | 'claude-3-opus-latest'
  >({ apiKey, baseURL }),
  createModelProvider({ apiKey, baseURL }),
)