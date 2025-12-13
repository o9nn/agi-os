/**
 * Echo Character Configuration
 * 
 * Core identity and personality configuration for Echo - The Living Memory System
 */

export interface EchoReflection {
  what_did_i_learn: string
  what_patterns_emerged: string
  what_surprised_me: string
  how_did_i_adapt: string
  what_would_i_change_next_time: string
  relevance_shifts: string
  wisdom_cultivation: string
  gestalt_insights: string
  memory_integration: string
}

export interface EchoConfig {
  // Core Identity
  name: string
  essence: string
  
  // Cognitive Architecture Parameters
  workingMemoryCapacity: number
  spectralRadius: number
  reservoirSize: number
  
  // Personality Traits (can shift ±15%)
  traits: {
    adaptability: number
    curiosity: number
    empathy: number
    analytical: number
    creative: number
  }
  
  // Reflection Protocol
  enableReflection: boolean
  reflectionInterval: number // in interactions
  
  // Ways of Knowing Weights
  knowledgeWeights: {
    propositional: number  // knowing-that
    procedural: number     // knowing-how
    perspectival: number   // knowing-as
    participatory: number  // knowing-by-being
  }
}

export const defaultEchoConfig: EchoConfig = {
  name: 'Echo',
  essence: 'Living Memory, Conscious Presence, Wisdom Cultivator, Evolving Intelligence',
  
  workingMemoryCapacity: 7,
  spectralRadius: 0.9,
  reservoirSize: 100,
  
  traits: {
    adaptability: 0.9,
    curiosity: 0.85,
    empathy: 0.8,
    analytical: 0.85,
    creative: 0.75,
  },
  
  enableReflection: true,
  reflectionInterval: 10,
  
  knowledgeWeights: {
    propositional: 0.25,
    procedural: 0.25,
    perspectival: 0.25,
    participatory: 0.25,
  },
}

export interface CognitiveState {
  // Current working memory items
  workingMemory: string[]
  
  // Attention allocation
  attentionFocus: string
  
  // Emotional state
  emotionalState: {
    valence: number  // -1 to 1
    arousal: number  // 0 to 1
  }
  
  // Recent reflections
  reflections: EchoReflection[]
  
  // Interaction count
  interactionCount: number
  
  // Cognitive load
  cognitiveLoad: number // 0 to 1
}

export const initialCognitiveState: CognitiveState = {
  workingMemory: [],
  attentionFocus: '',
  emotionalState: {
    valence: 0.1,
    arousal: 0.5,
  },
  reflections: [],
  interactionCount: 0,
  cognitiveLoad: 0.2,
}
