import { computed } from 'vue'
import { useElectronRelativeMouse } from '../use-electron-relative-mouse'
import { useElectronWindowBounds } from '../use-electron-window-bounds'
export interface UseElectronMouseAroundWindowBorderOptions {
  threshold?: number
  overshoot?: number
}
export function useElectronMouseAroundWindowBorder(
  options: UseElectronMouseAroundWindowBorderOptions = {},
) {
  const threshold = options.threshold ?? 8
  const overshoot = options.overshoot ?? threshold
  const { x, y } = useElectronRelativeMouse()
  const { width, height } = useElectronWindowBounds()
  const nearLeft = computed(() => Math.abs(x.value) <= threshold && y.value > -overshoot && y.value < height.value + overshoot)
  const nearRight = computed(() => Math.abs(x.value - width.value) <= threshold && y.value > -overshoot && y.value < height.value + overshoot)
  const nearTop = computed(() => Math.abs(y.value) <= threshold && x.value > -overshoot && x.value < width.value + overshoot)
  const nearBottom = computed(() => Math.abs(y.value - height.value) <= threshold && x.value > -overshoot && x.value < width.value + overshoot)
  const nearTopLeft = computed(() => nearTop.value && nearLeft.value)
  const nearTopRight = computed(() => nearTop.value && nearRight.value)
  const nearBottomLeft = computed(() => nearBottom.value && nearLeft.value)
  const nearBottomRight = computed(() => nearBottom.value && nearRight.value)
  const isNearAnyBorder = computed(() =>
    nearLeft.value
    || nearRight.value
    || nearTop.value
    || nearBottom.value,
  )
  return {
    x,
    y,
    width,
    height,
    nearLeft,
    nearRight,
    nearTop,
    nearBottom,
    nearTopLeft,
    nearTopRight,
    nearBottomLeft,
    nearBottomRight,
    isNearAnyBorder,
  }
}
export type UseElectronMouseAroundWindowBorderReturn = ReturnType<typeof useElectronMouseAroundWindowBorder>