import { describe, expect, it } from 'vitest'
import { err, ok } from '../core'
import { unwrap, unwrapErr, unwrapOr, unwrapOrElse } from './unwrap'
describe('@moeru/results', () => {
it('result.unwrap', () => {
expect(unwrap(ok(2))).toBe(2)
expect(() => unwrap(err('emergency failure'))).toThrowErrorMatchingSnapshot()
})
it('result.unwrapErr', () => {
expect(() => unwrapErr(ok(2))).toThrowErrorMatchingSnapshot()
expect(unwrapErr(err('emergency failure'))).toBe('emergency failure')
})
it('result.unwrapOr', () => {
expect(unwrapOr(ok(9), 2)).toBe(9)
expect(unwrapOr(err('error'), 2)).toBe(2)
})
it('result.unwrapOrElse', () => {
const count = (x: string) => x.length
expect(unwrapOrElse<number, string>(ok(2), count)).toBe(2)
expect(unwrapOrElse(err('foo'), count)).toBe(3)
})
})