import { LoaderUtils } from 'three'
export const resolveResourcePath = (url: string, resourcePath: string, path: string) =>
  resourcePath !== ''
    ? resourcePath
    : path !== ''
      ? path
      : LoaderUtils.extractUrlBase(url)