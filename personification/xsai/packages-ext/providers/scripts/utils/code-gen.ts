import { camelCase, pascalCase } from 'scule'
import type { CodeGenProvider } from './types'
const codeGenConstCreate = (provider: CodeGenProvider) => `create${provider.overrides?.create ?? pascalCase(provider.id)}`
const codeGenConstEntry = (provider: CodeGenProvider) => provider.overrides?.id ?? camelCase(provider.id)
export const codeGenCreate = (provider: CodeGenProvider) => [
'',
`export const ${codeGenConstCreate(provider)} = (apiKey: string, baseURL = '${provider.baseURL}') => merge(`,
`  createChatProvider${provider.models.length > 0 ? `<'${provider.models.join('\' | \'')}'>` : ''}({ apiKey, baseURL }),`,
...(provider.capabilities?.model === false ? [] : ['  createModelProvider({ apiKey, baseURL }),']),
...(provider.capabilities?.embed === true ? ['  createEmbedProvider({ apiKey, baseURL }),'] : []),
...(provider.capabilities?.image === true ? ['  createImageProvider({ apiKey, baseURL }),'] : []),
...(provider.capabilities?.speech === true ? ['  createSpeechProvider({ apiKey, baseURL }),'] : []),
...(provider.capabilities?.transcription === true ? ['  createTranscriptionProvider({ apiKey, baseURL }),'] : []),
')',
].join('\n')
export const codeGenIndex = (provider: CodeGenProvider) => {
const create = codeGenConstCreate(provider)
return {
ex: [
'',
`export const ${codeGenConstEntry(provider)} = ${create}(process.env.${provider.apiKey.join(' ?? process.env.')} ?? '')`,
].join('\n'),
im: create,
}
}
export const codeGenTypes = (provider: CodeGenProvider) => `export type ${pascalCase(provider.id)}Models = '${provider.models.join('\' | \'')}'`