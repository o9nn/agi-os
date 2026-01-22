import { createChatProvider, createMetadataProvider, createModelProvider, merge } from '@xsai-ext/shared-providers'
export const createMoonshot = (apiKey: string, baseURL = 'https://api.moonshot.cn/v1/') => merge(
createMetadataProvider('moonshot'),
createChatProvider<
| 'moonshot-v1-8k'
| 'moonshot-v1-8k-vision-preview'
| 'moonshot-v1-32k'
| 'moonshot-v1-32k-vision-preview'
| 'moonshot-v1-128k'
| 'moonshot-v1-128k-vision-preview'
| 'moonshot-v1-auto'
>({ apiKey, baseURL }),
createModelProvider({ apiKey, baseURL }),
)