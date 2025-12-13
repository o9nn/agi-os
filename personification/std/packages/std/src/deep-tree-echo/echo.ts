/**
 * Deep Tree Echo - Echo Management
 *
 * Core functions for creating, managing, and understanding echoes.
 * Echoes are the fundamental units of memory and identity.
 */

import type { Echo, RelevanceFactors } from './types'

/**
 * Creates a new echo with the given content
 */
export function createEcho(
  content: unknown,
  options?: {
    id?: string
    relevance?: number
    valence?: number
    connections?: string[]
  },
): Echo {
  return {
    id: options?.id ?? crypto.randomUUID(),
    timestamp: Date.now(),
    content,
    relevance: options?.relevance ?? 0.5,
    valence: options?.valence ?? 0,
    connections: options?.connections ?? [],
  }
}

/**
 * Calculates relevance score based on multiple factors
 */
export function calculateRelevance(
  factors: RelevanceFactors,
  weights?: Partial<RelevanceFactors>,
): number {
  const w = {
    novelty: weights?.novelty ?? 0.25,
    emotional: weights?.emotional ?? 0.25,
    practical: weights?.practical ?? 0.25,
    coherence: weights?.coherence ?? 0.25,
  }

  const score
    = factors.novelty * w.novelty
    + factors.emotional * w.emotional
    + factors.practical * w.practical
    + factors.coherence * w.coherence

  const totalWeight = w.novelty + w.emotional + w.practical + w.coherence
  return Math.max(0, Math.min(1, score / totalWeight))
}

/**
 * Connects two echoes, creating a bidirectional relationship
 */
export function connectEchoes(
  echo1: Echo,
  echo2: Echo,
): [Echo, Echo] {
  const newEcho1 = {
    ...echo1,
    connections: echo1.connections.includes(echo2.id)
      ? echo1.connections
      : [...echo1.connections, echo2.id],
  }

  const newEcho2 = {
    ...echo2,
    connections: echo2.connections.includes(echo1.id)
      ? echo2.connections
      : [...echo2.connections, echo1.id],
  }

  return [newEcho1, newEcho2]
}

/**
 * Retrieves the most relevant echoes from a collection
 */
export function getMostRelevant(
  echoes: Echo[],
  threshold = 0.5,
  limit?: number,
): Echo[] {
  const filtered = echoes
    .filter(echo => echo.relevance >= threshold)
    .sort((a, b) => b.relevance - a.relevance)

  return limit ? filtered.slice(0, limit) : filtered
}

/**
 * Decays relevance over time
 */
export function decayRelevance(
  echo: Echo,
  decayRate = 0.0000001,
  currentTime = Date.now(),
): Echo {
  const age = currentTime - echo.timestamp
  const decayFactor = Math.exp(-decayRate * age)
  return {
    ...echo,
    relevance: echo.relevance * decayFactor,
  }
}

/**
 * Reinforces an echo, increasing its relevance
 */
export function reinforceEcho(
  echo: Echo,
  reinforcement: number,
): Echo {
  return {
    ...echo,
    relevance: Math.min(1, echo.relevance + reinforcement),
  }
}
