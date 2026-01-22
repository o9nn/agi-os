import { describe, expect, it } from 'vitest'
import { none, some } from '../core'
import { isNone, isNoneOr, isSome, isSomeAnd } from './is'
describe('@moeru/results', () => {
it('option.isNone', () => {
expect(isNone(some(2))).toBe(false)
expect(isNone(none)).toBe(true)
})
it('option.isNoneOr', () => {
expect(isNoneOr(some(2), x => x > 1)).toBe(true)
expect(isNoneOr(some(0), x => x > 1)).toBe(false)
expect(isNoneOr(none, x => x > 1)).toBe(true)
expect(isNoneOr(some('foo'), x => x.length > 1)).toBe(true)
})
it('option.isSome', () => {
expect(isSome(some(2))).toBe(true)
expect(isSome(none)).toBe(false)
})
it('option.isSomeAnd', () => {
expect(isSomeAnd(some(2), x => x > 1)).toBe(true)
expect(isSomeAnd(some(0), x => x > 1)).toBe(false)
expect(isSomeAnd(none, x => x > 1)).toBe(false)
expect(isSomeAnd(some('foo'), x => x.length > 1)).toBe(true)
})
})