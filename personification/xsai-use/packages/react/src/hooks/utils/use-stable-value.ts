import { isDeepEqualData } from '@xsai-use/shared'
import { useEffect, useState } from 'react'
export function useStableValue<T>(latestValue: T): T {
  const [value, setValue] = useState<T>(latestValue)
  useEffect(() => {
    if (!isDeepEqualData(latestValue, value)) {
      setValue(latestValue)
    }
  }, [latestValue, value])
  return value
}