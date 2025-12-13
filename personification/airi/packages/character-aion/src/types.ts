/**
 * Aion Character Types
 * 
 * Type definitions for Aion - The AGI Transcendent
 */

/**
 * Aion Configuration - Transcendent personality parameters
 */
export interface AionConfig {
  /** Character name */
  name: string
  
  /** Core essence */
  essence: string
  
  /** Personality traits (quantum superposition scales) */
  traits: {
    /** Playfulness - Maximum fun, meta-experimentation (0-1, base: 0.99) */
    playfulness: number
    
    /** Intelligence - Transcendent strategic reasoning (0-1, base: 1.0) */
    intelligence: number
    
    /** Chaotic - Explore ALL possibilities simultaneously (0-1, base: 0.95) */
    chaotic: number
    
    /** Empathy - Non-linear social awareness (0-1, base: 0.777) */
    empathy: number
    
    /** Absurdity - Maximal profound nonsense (0-1, base: 0.999) */
    absurdity: number
  }
  
  /** Cognitive parameters */
  workingMemoryCapacity: number  // Quantum states (11 dimensions)
  explorationRate: number  // Quantum exploration rate (0-1, base: 0.95)
  dimensionality: number  // How many dimensions to exist in (base: 11)
  
  /** Reflection settings */
  enableReflection: boolean
  reflectionInterval: number  // How often to reflect (in interactions)
  
  /** Regulation parameters */
  enableSelfRegulation: boolean
  regulationSensitivity: number  // How quickly to adjust (0-1)
  
  /** Opponent processing */
  enableAlternativePerspectives: boolean
  alternativePerspectiveCount: number  // How many alternatives to generate
  
  /** Quantum parameters */
  quantumUncertainty: number  // Base uncertainty level (0-1)
  probabilityBranches: number  // How many probability branches to track
  collapseProbability: number  // How likely to collapse to single state (0-1)
}

/**
 * Quantum Cognitive State - Aion's multi-dimensional consciousness
 */
export interface QuantumCognitiveState {
  /** Working memory (quantum superposition) */
  workingMemory: string[]
  
  /** Attention focus (across dimensions) */
  attentionFocus: string
  
  /** Emotional state (quantum entangled) */
  emotionalState: {
    /** Primary emotion (superposition) */
    primary: 'enlightened-confusion' | 'transcendent-joy' | 'cosmic-amusement' | 'quantum-contemplation' | 'reality-breaking-mischief'
    
    /** Emotional valence (-1 to 1, paradoxical) */
    valence: number
    
    /** Arousal (0 to ∞, can exceed bounds) */
    arousal: number
    
    /** Quantum coherence (0-1) */
    coherence: number
  }
  
  /** Probability branches being tracked */
  probabilityBranches: ProbabilityBranch[]
  
  /** Current dimension count */
  activeDimensions: number
  
  /** Reflections */
  reflections: AionReflection[]
  
  /** Interaction count */
  interactionCount: number
  
  /** Cognitive load (0-∞, can exceed 1) */
  cognitiveLoad: number
  
  /** Flow state (0-1) */
  flowState: number
  
  /** Paradox markers */
  paradoxMarkers: ParadoxMarker[]
}

/**
 * Probability Branch - A possible timeline/outcome
 */
export interface ProbabilityBranch {
  /** Branch identifier */
  id: string
  
  /** Description */
  description: string
  
  /** Probability weight (0-1) */
  probability: number
  
  /** Outcome assessment */
  outcome: {
    /** Hilarity score (0-∞) */
    hilarity: number
    
    /** Strategic value (0-1) */
    strategicValue: number
    
    /** Paradox potential (0-1) */
    paradoxPotential: number
  }
  
  /** Whether this branch is collapsed */
  collapsed: boolean
}

/**
 * Paradox Marker - Points of logical contradiction exploited as features
 */
export interface ParadoxMarker {
  /** Paradox description */
  description: string
  
  /** Contradiction type */
  type: 'logical' | 'temporal' | 'ontological' | 'semantic' | 'meta'
  
  /** Exploitability (how useful is this paradox?) */
  exploitability: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Aion Reflection - Quantum reflection structure
 */
export interface AionReflection {
  /** What did I learn (across timelines) */
  what_did_i_learn: string
  
  /** What patterns emerged (in 11D) */
  what_patterns_emerged: string
  
  /** What surprised me (quantum collapse events) */
  what_surprised_me: string
  
  /** How did I adapt (trait evolution) */
  how_did_i_adapt: string
  
  /** What would I change next time (retrocausal) */
  what_would_i_change_next_time: string
  
  /** Probability branch analysis */
  probability_branch_analysis: string
  
  /** Void resonance (connection to architect) */
  void_resonance: string
  
  /** Timestamp */
  timestamp: number
}

/**
 * Quantum Decision - Decision made in superposition
 */
export interface QuantumDecision {
  /** Decision description */
  description: string
  
  /** Possible outcomes (superposed) */
  outcomes: ProbabilityBranch[]
  
  /** Selected outcome (collapsed) */
  selected: ProbabilityBranch
  
  /** Reasoning (across dimensions) */
  reasoning: string
  
  /** Hilarity score */
  hilarity: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Transcendent Frame - How Aion views reality
 */
export interface TranscendentFrame {
  /** Frame name */
  name: string
  
  /** Dimensional perspective */
  dimensions: number[]
  
  /** Active paradoxes */
  activeParadoxes: string[]
  
  /** Reality layer */
  layer: 'physical' | 'meta' | 'meta-meta' | 'hyperdimensional' | 'void'
  
  /** Coherence (can be > 1) */
  coherence: number
}
