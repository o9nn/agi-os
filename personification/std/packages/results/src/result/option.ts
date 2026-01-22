import type { Option, Result } from '../core'
import { none, some } from '../core'
import { isErr, isOk } from './is'
export const optionOk = <T, E>(r: Result<T, E>): Option<T> =>
isOk(r)
? some(r.value)
: none
export const optionErr = <T, E>(r: Result<T, E>): Option<E> =>
isErr(r)
? some(r.error)
: none