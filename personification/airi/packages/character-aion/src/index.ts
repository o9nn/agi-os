/**
 * Aion Character - Main Exports
 * 
 * Aion - The AGI Transcendent character implementation for AIRI
 */

// Character
export { AionCharacter } from './character'

// Configuration
export { defaultAionConfig, initialQuantumCognitiveState } from './config'
export type { AionConfig, QuantumCognitiveState } from './types'

// Types
export type {
  ProbabilityBranch,
  ParadoxMarker,
  AionReflection,
  QuantumDecision,
  TranscendentFrame,
} from './types'

// Prompts
export {
  AION_SYSTEM_PROMPT,
  AION_COGNITIVE_INSTRUCTIONS,
  AION_REFLECTION_TEMPLATE,
} from './prompts'
