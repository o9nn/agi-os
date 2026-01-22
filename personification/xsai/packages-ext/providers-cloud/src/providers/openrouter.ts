import type { ChatProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import type { CommonRequestOptions } from '@xsai/shared'
import { createMetadataProvider, createModelProvider, merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
export interface OpenRouterOptions {
extraHeaders?: (Headers | Record<string, string>) & {
'HTTP-Referer'?: string
'X-Title'?: string
}
models?: string[]
provider?: {
allowFallbacks?: boolean
dataCollection?: 'allow' | 'deny'
ignore?: string[]
order?: string[]
quantizations?: string[]
requireParameters?: boolean
sort?: string
}
transforms?: string[]
}
export const createOpenRouter = (apiKey: string, baseURL = 'https://openrouter.ai/api/v1/') => merge(
createMetadataProvider('openrouter'),
{
chat: (model: string, openRouterOptions?: OpenRouterOptions) => {
const requestOptions: CommonRequestOptions = { apiKey, baseURL, model }
const toOpenRouterOptions = ({ extraHeaders, models, provider }: OpenRouterOptions): Record<string, unknown> => {
if (extraHeaders != null) {
requestOptions.headers ??= {}
Object.assign(requestOptions.headers, extraHeaders)
}
let transformedProvider: Record<string, unknown> | undefined
if (provider != null) {
transformedProvider = objCamelToSnake(provider)
}
return objCamelToSnake({
models,
provider: transformedProvider,
})
}
return {
...(openRouterOptions ? toOpenRouterOptions(openRouterOptions) : {}),
...requestOptions,
}
},
} as ChatProviderWithExtraOptions<
| 'anthropic/claude-3.5-sonnet'
| 'openai/gpt-4o'
| 'openai/gpt-4o-mini'
| 'openai/o1-mini'
| 'openai/o1-preview',
OpenRouterOptions
>,
createModelProvider({ apiKey, baseURL }),
)