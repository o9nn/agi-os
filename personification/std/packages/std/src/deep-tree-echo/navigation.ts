import type { NavigationState } from './types'

export function createNavigation(
  current: unknown,
  destination: unknown,
  options?: {
    confidence?: number
    path?: unknown[]
  },
): NavigationState {
  return {
    current,
    destination,
    path: options?.path ?? [current],
    confidence: options?.confidence ?? 0.5,
  }
}

export function updateNavigation(
  nav: NavigationState,
  newPosition: unknown,
  confidenceAdjustment = 0,
): NavigationState {
  return {
    ...nav,
    current: newPosition,
    path: [...nav.path, newPosition],
    confidence: Math.max(
      0,
      Math.min(1, nav.confidence + confidenceAdjustment),
    ),
  }
}

export function calculateProgress(
  nav: NavigationState,
): number {
  if (typeof nav.current === 'number' && typeof nav.destination === 'number') {
    const start = nav.path[0] as number
    const total = nav.destination - start
    if (total === 0)
      return 1
    return Math.max(0, Math.min(1, (nav.current - start) / total))
  }

  return Math.min(1, nav.path.length / 10)
}

export function hasReachedDestination(
  nav: NavigationState,
  tolerance = 0.01,
): boolean {
  if (typeof nav.current === 'number' && typeof nav.destination === 'number') {
    return Math.abs(nav.current - nav.destination) < tolerance
  }

  return JSON.stringify(nav.current) === JSON.stringify(nav.destination)
}

export function recalibrateDestination(
  nav: NavigationState,
  newDestination: unknown,
  resetPath = false,
): NavigationState {
  return {
    ...nav,
    destination: newDestination,
    path: resetPath ? [nav.current] : nav.path,
    confidence: resetPath ? 0.5 : nav.confidence * 0.8,
  }
}

export function findWayBack(
  nav: NavigationState,
  targetIndex: number,
): NavigationState {
  const actualIndex = targetIndex < 0 ? nav.path.length + targetIndex : targetIndex

  if (actualIndex < 0 || actualIndex >= nav.path.length) {
    return nav
  }

  return {
    ...nav,
    current: nav.path[actualIndex],
    path: nav.path.slice(0, actualIndex + 1),
    confidence: nav.confidence * 0.7,
  }
}

export function getDirection(
  nav: NavigationState,
): 'forward' | 'backward' | 'stationary' {
  if (nav.path.length < 2)
    return 'stationary'

  if (typeof nav.current === 'number' && typeof nav.destination === 'number') {
    const prev = nav.path[nav.path.length - 2] as number
    const curr = nav.current

    if (Math.abs(curr - prev) < 0.001)
      return 'stationary'

    const towardsDestination = nav.destination > prev
    const actualMovement = curr > prev

    return towardsDestination === actualMovement ? 'forward' : 'backward'
  }

  return 'stationary'
}

export function validatePath(
  nav: NavigationState,
  validator: (step: unknown) => boolean,
): boolean {
  return nav.path.every(validator)
}
