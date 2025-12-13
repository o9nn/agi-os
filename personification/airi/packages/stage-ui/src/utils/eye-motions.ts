const EYE_SACCADE_INT_STEP = 400
const EYE_SACCADE_INT_P: [number, number][] = [
  [0.075, 800],
  [0.110, 0],
  [0.125, 0],
  [0.140, 0],
  [0.125, 0],
  [0.050, 0],
  [0.040, 0],
  [0.030, 0],
  [0.020, 0],
  [1.000, 0],
]
for (let i = 1; i < EYE_SACCADE_INT_P.length; i++) {
  const prev = EYE_SACCADE_INT_P[i - 1]!
  const curr = EYE_SACCADE_INT_P[i]!
  curr[0] += prev[0]
  curr[1] = prev[1] + EYE_SACCADE_INT_STEP
}

/**
 * This is a simple function to generate a random interval between eye saccades.
 *
 * @returns Interval in milliseconds
 */
export function randomSaccadeInterval(): number {
  const r = Math.random()
  for (let i = 0; i < EYE_SACCADE_INT_P.length; i++) {
    const entry = EYE_SACCADE_INT_P[i]!
    if (r <= entry[0]) {
      return entry[1] + Math.random() * EYE_SACCADE_INT_STEP
    }
  }
  const last = EYE_SACCADE_INT_P[EYE_SACCADE_INT_P.length - 1]!
  return last[1] + Math.random() * EYE_SACCADE_INT_STEP
}
