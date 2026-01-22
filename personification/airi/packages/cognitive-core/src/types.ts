export interface FourWaysOfKnowing {
  propositional: number
  procedural: number
  perspectival: number
  participatory: number
}
export interface CognitiveContext {
  agentId: string
  task?: string
  environment: {
    type: 'minecraft' | 'factorio' | 'discord' | 'telegram' | 'twitter' | 'web' | 'other'
    state?: Record<string, unknown>
  }
  emotional: {
    valence: number  
    arousal: number  
  }
  workingMemory: string[]
  attentionFocus?: string
  recentHistory?: unknown[]
  timestamp: number
}
export interface RelevanceScore {
  overall: number
  components: {
    novelty: number
    emotional: number
    pragmatic: number
    coherence: number
    epistemic: number
  }
  confidence: number
  reasoning?: string
}
export interface Possibility {
  id: string
  description: string
  type: 'action' | 'thought' | 'perception' | 'memory' | 'other'
  data?: Record<string, unknown>
  cost?: number
}
export interface RankedPossibilities {
  items: Array<{
    possibility: Possibility
    relevance: RelevanceScore
  }>
  context: CognitiveContext
  timestamp: number
}
export interface TradeoffSpectrum {
  name: string
  description: string
  left: {
    label: string
    value: number  
  }
  right: {
    label: string
    value: number  
  }
  current: number  
  optimal?: number  
}
export interface SophrosyneDecision {
  spectrum: TradeoffSpectrum
  recommendation: number  
  reasoning: string
  confidence: number
  context: CognitiveContext
}
export interface AlternativeFrame {
  description: string
  differences: string[]
  strengths: string[]
  weaknesses: string[]
  plausibility: number  
}
export interface DialecticalSynthesis {
  thesis: string
  antithesis: AlternativeFrame
  synthesis: string
  preserved: string[]
  integrated: string[]
  transcended: string[]
  quality: number  
}
export interface CognitiveEvent {
  id: string
  type: 'perception' | 'action' | 'thought' | 'emotion' | 'reflection' | 'decision' | 'other'
  agentId: string
  description: string
  data?: Record<string, unknown>
  context: CognitiveContext
  timestamp: number
  relevance?: RelevanceScore
}
export interface Reflection {
  id: string
  agentId: string
  period: {
    start: number
    end: number
  }
  content: {
    learned: string
    patterns: string
    surprises: string
    adaptations: string
    improvements: string
    relevanceShifts: string
    wisdomCultivation: string
    gestaltInsights: string
    memoryIntegration: string
  }
  fourWays?: FourWaysOfKnowing
  timestamp: number
}
export interface WisdomAssessment {
  morality: number  
  meaning: number  
  mastery: number  
  overall: number  
  timestamp: number
  context?: CognitiveContext
}
export interface CognitiveFrame {
  id: string
  name: string
  description: string
  saliencePatterns: string[]
  blindSpots: string[]
  domain: string
  activation: number
  fitness?: number
}
export interface SalienceMap {
  items: SalientItem[]
  activeFrame: CognitiveFrame
  context: CognitiveContext
  timestamp: number
}
export interface SalientItem {
  id: string
  description: string
  salience: number
  reason: string
  factors: {
    frameBased: number
    goalBased: number
    noveltyBased: number
    emotionalBased: number
  }
}
export interface GripStrength {
  level: number
  optimal: number
  gap: number
  quality: 'too_abstract' | 'optimal' | 'too_concrete' | 'unknown'
  recommendation?: string
  confidence: number
}
export interface Gestalt {
  id: string
  description: string
  parts: string[]
  emergentProperties: string[]
  coherence: number
  stability: number
  alternatives?: Gestalt[]
}
export interface FrameShift {
  from: CognitiveFrame
  to: CognitiveFrame
  trigger: 'explicit' | 'anomaly' | 'goal_change' | 'learning' | 'social' | 'spontaneous'
  triggerDescription?: string
  quality?: number  
  timestamp: number
}
export interface OptimalGripAssessment {
  gripStrength: GripStrength
  activeFrame: CognitiveFrame
  salienceMap: SalienceMap
  gestalts: Gestalt[]
  recentShifts: FrameShift[]
  perspectivalFitness: number
  recommendations: string[]
  context: CognitiveContext
  timestamp: number
}
export interface FrameLibrary {
  frames: CognitiveFrame[]
  defaultFrame: CognitiveFrame
  domainMappings: Record<string, string[]>  
}