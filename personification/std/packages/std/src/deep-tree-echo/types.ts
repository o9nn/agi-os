export interface Echo {
  id: string
  timestamp: number
  content: unknown
  relevance: number
  valence: number
  connections: string[]
}
export interface WaysOfKnowing {
  propositional: number
  procedural: number
  perspectival: number
  participatory: number
}
export interface AdaptiveTrait {
  value: number
  baseline: number
  min: number
  max: number
}
export interface GestaltPattern {
  id: string
  components: string[]
  emergentProperties: Record<string, unknown>
  coherence: number
}
export interface NavigationState {
  current: unknown
  destination: unknown
  path: unknown[]
  confidence: number
}
export interface RelevanceFactors {
  novelty: number
  emotional: number
  practical: number
  coherence: number
}