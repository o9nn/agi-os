import type { WaysOfKnowing } from './types'

export function createWaysOfKnowing(
  overrides?: Partial<WaysOfKnowing>,
): WaysOfKnowing {
  const base = {
    propositional: 0.25,
    procedural: 0.25,
    perspectival: 0.25,
    participatory: 0.25,
  }

  const combined = { ...base, ...overrides }

  const total
    = combined.propositional
    + combined.procedural
    + combined.perspectival
    + combined.participatory

  return {
    propositional: combined.propositional / total,
    procedural: combined.procedural / total,
    perspectival: combined.perspectival / total,
    participatory: combined.participatory / total,
  }
}

export function balanceWaysOfKnowing(
  current: WaysOfKnowing,
  rate = 0.1,
): WaysOfKnowing {
  const target = 0.25
  const balanced = {
    propositional: current.propositional + (target - current.propositional) * rate,
    procedural: current.procedural + (target - current.procedural) * rate,
    perspectival: current.perspectival + (target - current.perspectival) * rate,
    participatory: current.participatory + (target - current.participatory) * rate,
  }

  return createWaysOfKnowing(balanced)
}

export function emphasizeWay(
  current: WaysOfKnowing,
  way: keyof WaysOfKnowing,
  amount: number,
): WaysOfKnowing {
  const adjusted = { ...current }
  const increase = Math.min(amount, 1 - adjusted[way])

  adjusted[way] += increase

  const others = (Object.keys(adjusted) as Array<keyof WaysOfKnowing>).filter(
    k => k !== way,
  )
  const decreaseEach = increase / others.length

  for (const other of others) {
    adjusted[other] = Math.max(0, adjusted[other] - decreaseEach)
  }

  return createWaysOfKnowing(adjusted)
}

export function calculateWisdomScore(
  knowing: WaysOfKnowing,
): number {
  const values = [
    knowing.propositional,
    knowing.procedural,
    knowing.perspectival,
    knowing.participatory,
  ]

  const mean = 0.25
  const variance
    = values.reduce((sum, v) => sum + (v - mean) ** 2, 0) / values.length
  const balanceScore = 1 - Math.min(1, variance * 10)

  const minValue = Math.min(...values)
  const depthScore = minValue / 0.25

  return (balanceScore * 0.6 + depthScore * 0.4)
}

export function getDominantWay(
  knowing: WaysOfKnowing,
): keyof WaysOfKnowing {
  const entries = Object.entries(knowing) as Array<
    [keyof WaysOfKnowing, number]
  >
  return entries.reduce((max, [key, val]) =>
    val > knowing[max] ? key : max, entries[0][0],
  )
}

export function isBalanced(
  knowing: WaysOfKnowing,
  threshold = 0.1,
): boolean {
  const target = 0.25
  const values = [
    knowing.propositional,
    knowing.procedural,
    knowing.perspectival,
    knowing.participatory,
  ]

  return values.every(v => Math.abs(v - target) <= threshold)
}

export function integrateLearning(
  current: WaysOfKnowing,
  learning: Partial<WaysOfKnowing>,
  rate = 0.1,
): WaysOfKnowing {
  const integrated = {
    propositional:
      current.propositional
      + (learning.propositional ?? 0 - current.propositional) * rate,
    procedural:
      current.procedural + (learning.procedural ?? 0 - current.procedural) * rate,
    perspectival:
      current.perspectival
      + (learning.perspectival ?? 0 - current.perspectival) * rate,
    participatory:
      current.participatory
      + (learning.participatory ?? 0 - current.participatory) * rate,
  }

  return createWaysOfKnowing(integrated)
}
