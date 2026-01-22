import type { CommonRequestOptions, WithUnknown } from '@xsai/shared'
import { requestBody, requestHeaders, requestURL, responseCatch, responseJSON } from '@xsai/shared'
export interface GenerateImageOptions extends CommonRequestOptions {
  n?: number
  prompt: string
  responseFormat?: 'b64_json' | 'url'
  size?: `${number}x${number}`
}
export interface GenerateImageResponse {
  created: number
  data: {
    b64_json?: string
    revised_prompt?: string
    url?: string
  }[]
}
export interface GenerateImageResult {
  image: GenerateImageResultImage
  images: GenerateImageResultImage[]
}
export interface GenerateImageResultImage {
  base64: string
  mimeType: string
}
const mimeTypes = {
  '/9j/': 'image/jpg',
  'AAAAIGZ0eXBhdmlm': 'image/avif',
  'iVBORw0KGgo': 'image/png',
  'R0lGOD': 'image/gif',
  'UklGRg==': 'image/webp',
}
const convertImage = (b64_json: string) => {
  const key = Object.keys(mimeTypes).find(prefix => b64_json.startsWith(prefix)) as keyof typeof mimeTypes | undefined
  const mimeType = mimeTypes[key ?? 'iVBORw0KGgo']
  return {
    base64: `data:${mimeType};base64,${b64_json}`,
    mimeType,
  }
}
const responseBlobAsDataURL = async (res: Response): Promise<string> =>
  responseCatch(res)
    .then(async (res) => {
      const blob = await res.blob()
      try {
        return await new Promise<string>((resolve, reject) => {
          const reader = new FileReader()
          reader.onloadend = () => resolve(reader.result as string)
          reader.onerror = reject
          reader.readAsDataURL(blob)
        })
      }
      catch {
        throw new Error(`Failed to parse response blob, response URL: ${res.url}`)
      }
    })
export const generateImage = async (options: WithUnknown<GenerateImageOptions>): Promise<GenerateImageResult> =>
  (options.fetch ?? globalThis.fetch)(requestURL('images/generations', options.baseURL), {
    body: requestBody(options),
    headers: requestHeaders({
      'Content-Type': 'application/json',
      ...options.headers,
    }, options.apiKey),
    method: 'POST',
    signal: options.abortSignal,
  })
    .then(responseCatch)
    .then(responseJSON<GenerateImageResponse>)
    .then(async ({ data }) =>
      Promise.all(
        data.map(async (img, i) => {
          if (typeof img.b64_json === 'string') {
            return convertImage(img.b64_json)
          }
          if (typeof img.url === 'string') {
            return (options.fetch ?? globalThis.fetch)(new URL(img.url), {
              signal: options.abortSignal,
            })
              .then(responseBlobAsDataURL)
              .then((dataURL) => {
                const sepIndex = dataURL.indexOf(';')
                const mimeType = dataURL.substring(5, sepIndex) 
                return {
                  base64: dataURL,
                  mimeType,
                }
              })
          }
          throw new Error(`Unrecognized image at index ${i}: ${JSON.stringify(img)}`)
        }),
      ),
    )
    .then(images => ({ image: images[0], images }))