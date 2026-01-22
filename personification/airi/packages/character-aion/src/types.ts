export interface AionConfig {
  name: string
  essence: string
  traits: {
    playfulness: number
    intelligence: number
    chaotic: number
    empathy: number
    absurdity: number
  }
  workingMemoryCapacity: number  
  explorationRate: number  
  dimensionality: number  
  enableReflection: boolean
  reflectionInterval: number  
  enableSelfRegulation: boolean
  regulationSensitivity: number  
  enableAlternativePerspectives: boolean
  alternativePerspectiveCount: number  
  quantumUncertainty: number  
  probabilityBranches: number  
  collapseProbability: number  
}
export interface QuantumCognitiveState {
  workingMemory: string[]
  attentionFocus: string
  emotionalState: {
    primary: 'enlightened-confusion' | 'transcendent-joy' | 'cosmic-amusement' | 'quantum-contemplation' | 'reality-breaking-mischief'
    valence: number
    arousal: number
    coherence: number
  }
  probabilityBranches: ProbabilityBranch[]
  activeDimensions: number
  reflections: AionReflection[]
  interactionCount: number
  cognitiveLoad: number
  flowState: number
  paradoxMarkers: ParadoxMarker[]
}
export interface ProbabilityBranch {
  id: string
  description: string
  probability: number
  outcome: {
    hilarity: number
    strategicValue: number
    paradoxPotential: number
  }
  collapsed: boolean
}
export interface ParadoxMarker {
  description: string
  type: 'logical' | 'temporal' | 'ontological' | 'semantic' | 'meta'
  exploitability: number
  timestamp: number
}
export interface AionReflection {
  what_did_i_learn: string
  what_patterns_emerged: string
  what_surprised_me: string
  how_did_i_adapt: string
  what_would_i_change_next_time: string
  probability_branch_analysis: string
  void_resonance: string
  timestamp: number
}
export interface QuantumDecision {
  description: string
  outcomes: ProbabilityBranch[]
  selected: ProbabilityBranch
  reasoning: string
  hilarity: number
  timestamp: number
}
export interface TranscendentFrame {
  name: string
  dimensions: number[]
  activeParadoxes: string[]
  layer: 'physical' | 'meta' | 'meta-meta' | 'hyperdimensional' | 'void'
  coherence: number
}