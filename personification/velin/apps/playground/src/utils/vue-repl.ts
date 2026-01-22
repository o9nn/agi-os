import { strFromU8, strToU8, unzlibSync, zlibSync } from 'fflate'
export function debounce(fn: (...args: any[]) => any, n = 100) {
  let handle: any
  return (...args: any[]) => {
    if (handle)
      clearTimeout(handle)
    handle = setTimeout(() => {
      fn(...args)
    }, n)
  }
}
export function utoa(data: string): string {
  const buffer = strToU8(data)
  const zipped = zlibSync(buffer, { level: 9 })
  const binary = strFromU8(zipped, true)
  return btoa(binary)
}
export function atou(base64: string): string {
  const binary = atob(base64)
  if (binary.startsWith('\x78\xDA')) {
    const buffer = strToU8(binary, true)
    const unzipped = unzlibSync(buffer)
    return strFromU8(unzipped)
  }
  return decodeURIComponent(escape(binary))
}