export interface CodeGenProvider {
  apiKey: string[]
  baseURL: string
  capabilities?: CodeGenProviderCapabilities
  doc: string
  id: string
  models: string[]
  name: string
  overrides?: CodeGenProviderOverrides
}
export interface CodeGenProviderCapabilities {
  embed?: boolean
  image?: boolean
  model?: boolean
  speech?: boolean
  transcription?: boolean
}
export interface CodeGenProviderOverrides {
  create?: string
  id?: string
}
export interface Model {
  id: string
}
export interface Provider {
  _capabilities?: CodeGenProviderCapabilities
  _overrides?: CodeGenProviderOverrides
  api?: string
  doc: string
  env: string[]
  id: string
  models: Record<string, Model>
  name: string
  npm: string
}
export type Providers = Record<string, Provider>