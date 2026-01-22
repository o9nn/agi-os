import { createChatProvider, createMetadataProvider, merge } from '@xsai-ext/shared-providers'
type MinimaxModel = 'abab6.5s-chat' | 'DeepSeek-R1' | 'MiniMax-Text-01'
export const createMinimaxi = (apiKey: string, baseURL = 'https://api.minimaxi.chat/v1/') => merge(
  createMetadataProvider('minimaxi'),
  createChatProvider<MinimaxModel>({ apiKey, baseURL }),
)
export const createMinimax = (apiKey: string, baseURL = 'https://api.minimax.chat/v1/') => merge(
  createMetadataProvider('minimax'),
  createChatProvider<MinimaxModel>({ apiKey, baseURL }),
)