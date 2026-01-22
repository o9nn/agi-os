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
  name: string
  essence: string
  workingMemoryCapacity: number
  spectralRadius: number
  reservoirSize: number
  traits: {
    adaptability: number
    curiosity: number
    empathy: number
    analytical: number
    creative: number
  }
  enableReflection: boolean
  reflectionInterval: number 
  knowledgeWeights: {
    propositional: number  
    procedural: number     
    perspectival: number   
    participatory: number  
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
  workingMemory: string[]
  attentionFocus: string
  emotionalState: {
    valence: number  
    arousal: number  
  }
  reflections: EchoReflection[]
  interactionCount: number
  cognitiveLoad: number 
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