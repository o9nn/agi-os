import {
createChatProvider,
createModelProvider,
merge,
} from '@xsai-ext/shared-providers'
export const createFeatherless = (apiKey: string, baseURL = 'https://api.featherless.ai/v1/') => merge(
createChatProvider({ apiKey, baseURL }),
createModelProvider({
apiKey,
baseURL,
fetch: async (...args: Parameters<typeof globalThis.fetch>) => globalThis.fetch(...args)
.then(async res => res.json() as Promise<unknown[]>)
.then(data => Response.json({ data, object: 'list' })),
}),
)