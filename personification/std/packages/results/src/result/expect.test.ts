import { describe, expect, it } from 'vitest'
import { err, ok } from '../core'
import { expectErr, expect as expectOk } from './expect'
describe('@moeru/results', () => {
it('result.expect', () => {
expect(() => expectOk(err('emergency failure'), 'Testing expect')).toThrowErrorMatchingSnapshot()
})
it('result.expectErr', () => {
expect(() => expectErr(ok(10), 'Testing expectErr')).toThrowErrorMatchingSnapshot()
})
})