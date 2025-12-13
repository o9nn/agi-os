import type { AdaptiveTrait } from './types'

export function createAdaptiveTrait(
  baseline: number,
  adaptationBound = 0.15,
): AdaptiveTrait {
  return {
    value: baseline,
    baseline,
    min: Math.max(0, baseline - Math.abs(baseline * adaptationBound)),
    max: Math.min(1, baseline + Math.abs(baseline * adaptationBound)),
  }
}

export function adaptTrait(
  trait: AdaptiveTrait,
  target: number,
  rate = 0.1,
): AdaptiveTrait {
  const delta = (target - trait.value) * rate
  const newValue = trait.value + delta
  const clampedValue = Math.max(trait.min, Math.min(trait.max, newValue))

  return {
    ...trait,
    value: clampedValue,
  }
}

export function canAdaptTo(
  trait: AdaptiveTrait,
  value: number,
): boolean {
  return value >= trait.min && value <= trait.max
}

export function getAdaptationCapacity(
  trait: AdaptiveTrait,
): { upward: number, downward: number } {
  return {
    upward: trait.max - trait.value,
    downward: trait.value - trait.min,
  }
}

export function resetToBaseline(
  trait: AdaptiveTrait,
): AdaptiveTrait {
  return {
    ...trait,
    value: trait.baseline,
  }
}

export function isAtExtreme(
  trait: AdaptiveTrait,
  tolerance = 0.01,
): { atMin: boolean, atMax: boolean } {
  return {
    atMin: Math.abs(trait.value - trait.min) < tolerance,
    atMax: Math.abs(trait.value - trait.max) < tolerance,
  }
}
