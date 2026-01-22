import type { ChatProviderWithExtraOptions } from '@xsai-ext/shared-providers'
import type { CommonRequestOptions } from '@xsai/shared'
import { createModelProvider, merge } from '@xsai-ext/shared-providers'
import { objCamelToSnake } from '@xsai/shared'
import type { OpenrouterModels } from '../../generated/types'
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
OpenrouterModels,
OpenRouterOptions
>,
createModelProvider({ apiKey, baseURL }),
)