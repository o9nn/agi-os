import type { Fetch } from './fetch'
export interface CommonRequestOptions {
abortSignal?: AbortSignal
apiKey?: string
baseURL: string | URL
fetch?: Fetch | typeof globalThis.fetch
headers?: Headers | Record<string, string>
model: string
}