import type { Transform } from 'sucrase'
import { transform } from 'sucrase'
export function testTs(filename: string | undefined | null) {
return !!(filename && /(\.|\b)tsx?$/.test(filename))
}
export function transformTS(src: string, isJSX?: boolean) {
return transform(src, {
transforms: ['typescript', ...(isJSX ? (['jsx'] as Transform[]) : [])],
jsxRuntime: 'preserve',
}).code
}