import { createChatProvider, createEmbedProvider, createMetadataProvider, merge } from '@xsai-ext/shared-providers'
export const createWorkersAI = (apiKey: string, accountId: string) => {
  const baseURL = `https://api.cloudflare.com/client/v4/accounts/${accountId}/ai/v1/`
  return merge(
    createMetadataProvider('workers-ai'),
    createChatProvider({ apiKey, baseURL }),
    createEmbedProvider({ apiKey, baseURL }),
  )
}