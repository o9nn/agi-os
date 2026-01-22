import { describe, expect, it } from 'vitest'
import { none, some } from '../core'
import { or, orElse } from './or'
describe('@moeru/results', () => {
it('option.or', () => {
expect(or(some(2), none)).toStrictEqual(some(2))
expect(or(none, some(100))).toStrictEqual(some(100))
expect(or(some(2), some(100))).toStrictEqual(some(2))
expect(or(none, none)).toStrictEqual(none)
})
it('option.okOrElse', () => {
const nobody = () => none
const vikings = () => some('vikings')
expect(orElse(some('barbarians'), vikings)).toStrictEqual(some('barbarians'))
expect(orElse(none, vikings)).toStrictEqual(some('vikings'))
expect(orElse(none, nobody)).toStrictEqual(none)
})
})