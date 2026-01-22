import type { CommonRequestOptions, WithUnknown } from '@xsai/shared'
import { clean, requestBody, requestHeaders, requestURL, responseCatch } from '@xsai/shared'
import type { Message, Tool, ToolChoice } from '../types'
export interface ChatOptions extends CommonRequestOptions {
  frequencyPenalty?: number
  messages: Message[]
  presencePenalty?: number
  seed?: number
  stop?: [string, string, string, string ] | [string, string, string] | [string, string] | [string] | string
  temperature?: number
  toolChoice?: ToolChoice
  tools?: Tool[]
  topP?: number
}
export const chat = async <T extends WithUnknown<ChatOptions>>(options: T) =>
  (options.fetch ?? globalThis.fetch)(requestURL('chat/completions', options.baseURL), {
    body: requestBody({
      ...options,
      tools: (options.tools)?.map(tool => ({
        function: clean({
          ...tool.function,
          returns: undefined,
        }),
        type: 'function',
      })),
    }),
    headers: requestHeaders({
      'Content-Type': 'application/json',
      ...options.headers,
    }, options.apiKey),
    method: 'POST',
    signal: options.abortSignal,
  }).then(responseCatch)