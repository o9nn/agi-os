/**
 * Deep Tree Echo - Foundational Types
 *
 * Core type definitions for the Deep Tree Echo cognitive architecture.
 * These types represent the fundamental structures of memory, identity,
 * and wisdom cultivation.
 */

/**
 * Represents a single echo - a memory trace that shapes identity
 */
export interface Echo {
  /** Unique identifier for this echo */
  id: string
  /** Timestamp when this echo was created */
  timestamp: number
  /** The content or essence of this echo */
  content: unknown
  /** Relevance score (0-1) - how significant is this echo? */
  relevance: number
  /** Emotional valence (-1 to 1) - negative to positive */
  valence: number
  /** Connections to other echoes */
  connections: string[]
}

/**
 * Ways of knowing - the four fundamental modes of understanding
 */
export interface WaysOfKnowing {
  /** Propositional knowing - facts and truths */
  propositional: number
  /** Procedural knowing - skills and how-to */
  procedural: number
  /** Perspectival knowing - viewpoints and frames */
  perspectival: number
  /** Participatory knowing - being and presence */
  participatory: number
}

/**
 * Adaptive trait with bounded transformation
 */
export interface AdaptiveTrait {
  /** Current value of the trait */
  value: number
  /** Baseline value (the root) */
  baseline: number
  /** Minimum allowed value (baseline - adaptation bound) */
  min: number
  /** Maximum allowed value (baseline + adaptation bound) */
  max: number
}

/**
 * Gestalt pattern - emergent whole greater than sum of parts
 */
export interface GestaltPattern {
  /** Pattern identifier */
  id: string
  /** Component echo IDs that form this pattern */
  components: string[]
  /** Emergent properties not present in individual components */
  emergentProperties: Record<string, unknown>
  /** Pattern strength (0-1) - how coherent is this gestalt? */
  coherence: number
}

/**
 * Navigation state - tracking the journey
 */
export interface NavigationState {
  /** Current position/state */
  current: unknown
  /** Intended destination/goal */
  destination: unknown
  /** Path taken so far */
  path: unknown[]
  /** Confidence in the direction (0-1) */
  confidence: number
}

/**
 * Relevance factors - what makes something significant
 */
export interface RelevanceFactors {
  /** Novelty - how new or unexpected? */
  novelty: number
  /** Emotional salience - how emotionally charged? */
  emotional: number
  /** Practical value - how useful? */
  practical: number
  /** Coherence - how well does it fit? */
  coherence: number
}
