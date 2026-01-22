import { MutableRefObject, useEffect, useRef, useState } from 'react'
import { debounce } from 'debounce'
export function useDebounced<ARGS, RET>(
  func: (...any: ARGS[]) => RET,
  delay: number
): (...any: ARGS[]) => RET {
  return useState(() => debounce(func, delay))[0]
}
export function useRefLock(): {
  isLocked: () => boolean
  setLock: (lock: boolean) => void
} {
  const lockRef = useRef<boolean>(false)
  const stableRef = useRef<any>({
    isLocked: () => {
      return lockRef.current === true
    },
    setLock: (lock: boolean) => {
      return (lockRef.current = lock)
    },
  }) as MutableRefObject<any>
  return stableRef.current
}
export function useInitEffect(cb: () => void) {
  const init = useRef(false)
  useEffect(() => {
    if (!init.current) {
      cb()
      init.current = true
    }
  })
}