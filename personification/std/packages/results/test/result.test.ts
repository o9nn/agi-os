import type { Result } from '../src'
import { describe, expect, it } from 'vitest'
import { err, ok } from '../src'
import { match, unwrap, wrap } from '../src/result'
describe('@moeru/results/result', () => {
  enum MathError {
    DivisionByZero = 'DivisionByZero',
    NegativeSquareRoot = 'NegativeSquareRoot',
    NonPositiveLogarithm = 'NonPositiveLogarithm',
  }
  type MathResult = Result<number, MathError>
  const div = (x: number, y: number): MathResult =>
    y === 0
      ? err(MathError.DivisionByZero)
      : ok(x / y)
  const sqrt = (x: number): MathResult =>
    x < 0
      ? err(MathError.NegativeSquareRoot)
      : ok(Math.sqrt(x))
  const ln = (x: number): MathResult =>
    x <= 0
      ? err(MathError.NonPositiveLogarithm)
      : ok(Math.log(x))
  it('basic', () => {
    const op = (x: number, y: number): number =>
      match(
        div(x, y),
        radio => match(
          ln(radio),
          ln => match(
            sqrt(ln),
            sqrt => sqrt,
            (err) => { throw err },
          ),
          (err) => { throw err },
        ),
        (err) => { throw err },
      )
    expect(() => op(1, 10)).toThrowErrorMatchingSnapshot()
    expect(() => op(1, 0)).toThrowErrorMatchingSnapshot()
  })
  it('wrap', () => {
    const op = (x: number, y: number): number => match(
      wrap(() => {
        const _ratio = unwrap(div(x, y))
        const _ln = unwrap(ln(_ratio))
        return sqrt(_ln)
      }),
      value => value,
      (err) => { throw err },
    )
    expect(() => op(1, 10)).toThrowErrorMatchingSnapshot()
    expect(() => op(1, 0)).toThrowErrorMatchingSnapshot()
  })
})