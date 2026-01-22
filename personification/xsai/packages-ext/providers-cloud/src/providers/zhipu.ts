import {
createChatProvider,
createEmbedProvider,
createMetadataProvider,
createModelProvider,
merge,
} from '@xsai-ext/shared-providers'
export const createZhipu = (apiKey: string, baseURL = 'https://open.bigmodel.cn/api/paas/v4/') => merge(
createMetadataProvider('zhipu'),
createChatProvider<
| 'glm-4'
| 'glm-4v'
| 'glm-4v-plus'
| 'glm-zero-preview'
>({ apiKey, baseURL }),
createEmbedProvider<'embedding-3'>({ apiKey, baseURL }),
createModelProvider({ apiKey, baseURL }),
)