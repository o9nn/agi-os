import type { Fetch } from '@xsai/shared'
import {
createChatProvider,
createEmbedProvider,
createModelProvider,
createSpeechProvider,
createTranscriptionProvider,
merge,
} from '@xsai-ext/shared-providers'
import type { AzureModels } from '../../generated/types'
export interface CreateAzureOptions {
apiKey: (() => Promise<string> | string) | string
apiVersion?: string
resourceName: string
}
export const createAzure = async (options: CreateAzureOptions) => {
const headers = typeof options.apiKey === 'string'
? { 'api-key': options.apiKey }
: undefined
const baseURL = `https://${options.resourceName}.services.ai.azure.com/models/`
const fetch: Fetch = async (input, init) => {
if (options.apiVersion != null)
input.searchParams.set('api-version', options.apiVersion)
const token = `Bearer ${typeof options.apiKey === 'function' ? await options.apiKey() : options.apiKey}`
init.headers ??= {}
if (Array.isArray(init.headers))
init.headers.push(['Authorization', token])
else if (init.headers instanceof Headers)
init.headers.append('Authorization', token)
else
init.headers.Authorization = token
return globalThis.fetch(input, init)
}
return merge(
createChatProvider<AzureModels>({ baseURL, fetch, headers }),
createEmbedProvider({ baseURL, fetch, headers }),
createSpeechProvider({ baseURL, fetch, headers }),
createTranscriptionProvider({ baseURL, fetch, headers }),
createModelProvider({ baseURL, fetch, headers }),
)
}