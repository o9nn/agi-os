import { useRef } from 'react'
import { usePrevious } from './usePrevious'
export function useHasChanged(val: any) {
  const prevVal = usePrevious(val)
  return prevVal !== val
}
export function useHasChanged2(val: unknown, trueOnFirstRun = true): boolean {
  const prev = useRef(trueOnFirstRun ? Symbol() : val)
  if (prev.current !== val) {
    prev.current = val
    return true
  }
  return false
}