import { createChatProvider, createMetadataProvider, merge } from '@xsai-ext/shared-providers'
export const createPerplexity = (apiKey: string, baseURL = 'https://api.perplexity.ai/') => merge(
createMetadataProvider('perplexity'),
createChatProvider<'sonar' | 'sonar-pro' | 'sonar-reasoning' | 'sonar-reasoning-pro'>({ apiKey, baseURL }),
)