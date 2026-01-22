import {
createChatProvider,
createEmbedProvider,
createMetadataProvider,
createModelProvider,
merge,
} from '@xsai-ext/shared-providers'
export const createOllama = (baseURL = 'http://localhost:11434/v1/') => merge(
createMetadataProvider('ollama'),
createChatProvider<
| 'deepseek-r1'
| 'gemma3'
| 'granite3.3'
| 'llama3.1'
| 'llama3.2'
| 'llama3.2-vision'
| 'llama3.3'
| 'qwen2.5-coder'
| 'qwen3'
| 'qwq'
>({ baseURL }),
createEmbedProvider<'all-minilm' | 'granite-embedding' | 'mxbai-embed-large' | 'nomic-embed-text'>({ baseURL }),
createModelProvider({ baseURL }),
)