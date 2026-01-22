import { createChatProvider, createMetadataProvider, createModelProvider, merge } from '@xsai-ext/shared-providers'
export const createXAI = (apiKey: string, baseURL = 'https://api.x.ai/v1/') => merge(
createMetadataProvider('xai'),
createChatProvider<'grok-2-1212' | 'grok-2-vision-1212'>({ apiKey, baseURL }),
createModelProvider({ apiKey, baseURL }),
)