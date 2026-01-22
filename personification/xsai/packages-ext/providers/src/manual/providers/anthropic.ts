import {
  createChatProvider,
  createModelProvider,
  merge,
} from '@xsai-ext/shared-providers'
import type { AnthropicModels } from '../../generated/types'
export const createAnthropic = (apiKey: string, baseURL = 'https://api.anthropic.com/v1/') => merge(
  createChatProvider<AnthropicModels>({ apiKey, baseURL }),
  createModelProvider({ apiKey, baseURL }),
)