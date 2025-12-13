/**
 * @proj-airi/character-neuro
 * 
 * Neuro-Sama character implementation - chaotic cognitive VTuber with multi-agent orchestration
 */

export { NeuroCharacter } from './character'

export type {
  NeuroPersonality,
  NeuroCognitiveState,
  ConstraintWeights,
  CognitiveFrame,
  ActionOption,
  NeuroResponse,
  TheoryOfMindModel,
  NeuroReflection,
  SubordinateAgentConfig,
} from './types'

export {
  DEFAULT_NEURO_PERSONALITY,
  DEFAULT_CONSTRAINT_WEIGHTS,
  INITIAL_NEURO_STATE,
  FRAME_CONSTRAINT_WEIGHTS,
  FRAME_SELECTION_RULES,
  PERSONALITY_EVOLUTION_BOUNDS,
  REFLECTION_CONFIG,
  TOM_CONFIG,
  SAFETY_CONFIG,
  ROASTING_GUIDELINES,
  METACOGNITION_THRESHOLDS,
  SUBORDINATE_CONFIG,
  ATOMSPACE_CONFIG,
} from './config'

export {
  NEURO_SYSTEM_PROMPT,
  NEURO_COGNITIVE_INSTRUCTIONS,
  NEURO_REFLECTION_TEMPLATE,
} from './prompts'
