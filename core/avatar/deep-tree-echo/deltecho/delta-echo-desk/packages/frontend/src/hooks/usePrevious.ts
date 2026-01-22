import { useEffect, useRef } from 'react'
export function usePrevious(value: any) {
const ref = useRef()
useEffect(() => {
ref.current = value
})
return ref.current
}
export function usePrevious2<T>(val: T): T | undefined {
const prevRef = useRef<T>(undefined)
const prev = prevRef.current
prevRef.current = val
return prev
}