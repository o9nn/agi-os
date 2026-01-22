import type { BrowserWindow, Rectangle } from 'electron'
import { screen } from 'electron'
export function currentDisplayBounds(window: BrowserWindow) {
  const bounds = window.getBounds()
  const nearbyDisplay = screen.getDisplayMatching(bounds)
  return nearbyDisplay.bounds
}
interface SizeActual { actual: number }
interface SizePercentage { percentage: number }
type Size = SizeActual | SizePercentage | number
function evaluateSize(basedOn: number, size: Size) {
  if (typeof size === 'number') {
    return size
  }
  if ('actual' in size) {
    return size.actual
  }
  return Math.floor(basedOn * size.percentage)
}
export const tailwindBreakpoints = {
  'sm': { min: 640, max: 767 },
  'md': { min: 768, max: 1023 },
  'lg': { min: 1024, max: 1279 },
  'xl': { min: 1280, max: 1535 },
  '2xl': { min: 1536, max: 1791 },
  '3xl': { min: 1792, max: 2047 },
  '4xl': { min: 2048, max: 2303 },
  '5xl': { min: 2304, max: 2559 },
  '6xl': { min: 2560, max: 2815 },
  '7xl': { min: 2816, max: 3071 },
  '8xl': { min: 3072, max: 3327 },
  '9xl': { min: 3328, max: 3583 },
  '10xl': { min: 3584, max: Infinity },
}
export const resolutionBreakpoints = {
  '720p': { min: 0, max: 1280 },
  '1080p': { min: 1281, max: 1920 },
  '2k': { min: 1921, max: 2560 },
  '4k': { min: 2561, max: 3840 },
  '5k': { min: 3841, max: 7680 },
  '8k': { min: 7681, max: Infinity },
}
export function mapForBreakpoints<
  B extends Record<string, { min: number, max: number }> = typeof tailwindBreakpoints,
>(
  basedOn: number,
  sizes: { [key in keyof B]?: number } | number,
  options?: { breakpoints: B },
) {
  if (typeof sizes === 'number') {
    return sizes
  }
  const breakpoints = options?.breakpoints ?? tailwindBreakpoints
  const matched = Object.entries(breakpoints).find(([, b]) => {
    return basedOn >= b.min && basedOn <= b.max
  })
  if (matched) {
    const size = sizes[matched[0]]
    if (size) {
      return size
    }
  }
  const sortedSizes = Object.entries(sizes)
    .map(([key, value]) => ({ key, value, min: breakpoints[key as keyof typeof breakpoints]?.min ?? 0 }))
    .sort((a, b) => b.min - a.min) 
  const fallback = sortedSizes.find(s => s.min <= basedOn)
  return fallback?.value ?? Object.values(sizes)?.[0] ?? 0
}
export function widthFrom(bounds: Rectangle, sizeOptions: Size & { min?: Size, max?: Size }) {
  const val = evaluateSize(bounds.width, sizeOptions)
  const min = sizeOptions.min ? evaluateSize(bounds.width, sizeOptions.min) : undefined
  const max = sizeOptions.max ? evaluateSize(bounds.width, sizeOptions.max) : undefined
  if (min && val < min) {
    return min
  }
  if (max && val > max) {
    return max
  }
  return val
}
export function heightFrom(bounds: Rectangle, sizeOptions: Size & { min?: Size, max?: Size }) {
  const val = evaluateSize(bounds.height, sizeOptions)
  const min = sizeOptions.min ? evaluateSize(bounds.height, sizeOptions.min) : undefined
  const max = sizeOptions.max ? evaluateSize(bounds.height, sizeOptions.max) : undefined
  if (min && val < min) {
    return min
  }
  if (max && val > max) {
    return max
  }
  return val
}