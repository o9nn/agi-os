import {
createChatProvider,
createEmbedProvider,
createMetadataProvider,
createModelProvider,
merge,
} from '@xsai-ext/shared-providers'
export const createLMStudio = (baseURL = 'http://localhost:1234/v1/') => merge(
createMetadataProvider('lmstudio'),
createChatProvider({ baseURL }),
createEmbedProvider({ baseURL }),
createModelProvider({ baseURL }),
)