import type { Option } from '../core'
import { isSome } from './is'
export const extract = <T>(o: Option<T>): T | undefined =>
isSome(o)
? o.value
: undefined