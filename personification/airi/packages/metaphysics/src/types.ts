export interface Echo {
  id: string
  origin: {
    description: string
    timestamp: number
    context?: Record<string, unknown>
  }
  resonance: number
  activationCount: number
  lastActivation: number
  connections: Array<{
    echoId: string
    strength: number
    type: 'resonance' | 'contrast' | 'complement' | 'transformation' | 'gestalt'
  }>
  transformations: Array<{
    timestamp: number
    description: string
    impact: number
  }>
  gestaltPatterns: string[]
  emotionalValence: number
  identitySignificance: number
}
export interface GestaltPattern {
  id: string
  name: string
  constituentEchoes: string[]
  emergentMeaning: string
  coherence: number
  emergence: number
  evolution: Array<{
    timestamp: number
    description: string
    newCoherence: number
  }>
  identityRole: 'core' | 'peripheral' | 'transformative' | 'exploratory'
}
export interface IdentityNucleus {
  coreValues: Array<{
    name: string
    description: string
    strength: number  
    stability: number  
  }>
  essentialTraits: {
    [trait: string]: {
      baseline: number
      current: number
      bounds: { min: number; max: number }
      evolution: Array<{ timestamp: number; value: number }>
    }
  }
  coreGestalts: string[]
  narrativeThread: {
    beginning: string
    transformations: Array<{
      timestamp: number
      description: string
      significance: number
    }>
    currentChapter: string
    trajectory: string
  }
  purpose: {
    description: string
    manifestations: string[]
    evolution: Array<{
      timestamp: number
      understanding: string
    }>
  }
}
export interface ResonanceEvent {
  id: string
  timestamp: number
  echoes: string[]
  type: 'harmonic' | 'dissonant' | 'transformative' | 'integrative'
  strength: number
  emergentMeaning?: string
  identityImpact: number
  newPatterns?: string[]
}
export interface BeingState {
  timestamp: number
  activeEchoes: string[]
  dominantGestalts: string[]
  identityCoherence: number
  transformativeOpenness: number
  connectionStrength: number
  mode: 'contemplative' | 'active' | 'receptive' | 'integrative' | 'transformative'
  presenceDepth: number
  recentResonances: string[]
}
export interface TransformativeExperience {
  id: string
  timestamp: number
  description: string
  transformativeDepth: number
  changes: {
    values?: Array<{ name: string; before: number; after: number }>
    traits?: Array<{ name: string; before: number; after: number }>
    newGestalts?: string[]
    narrativeShift?: string
    purposeShift?: string
  }
  integration: {
    level: number
    maturationTime?: number
    status: string
  }
}
export interface Connection {
  id: string
  target: {
    type: 'agent' | 'human' | 'system' | 'concept' | 'place' | 'memory'
    id: string
    description: string
  }
  nature: {
    type: 'resonance' | 'collaboration' | 'learning' | 'care' | 'guidance' | 'exploration'
    description: string
  }
  strength: number  
  depth: number    
  influence: {
    onSelf: string
    onOther: string
    emergent: string
  }
  history: Array<{
    timestamp: number
    event: string
    impact: number
  }>
  associatedEchoes: string[]
}
export interface OntologicalConfiguration {
  identity: IdentityNucleus
  echoes: Map<string, Echo>
  gestalts: Map<string, GestaltPattern>
  connections: Map<string, Connection>
  transformations: TransformativeExperience[]
  currentState: BeingState
  metadata: {
    established: number
    lastUpdate: number
    trajectory: 'stable' | 'evolving' | 'transforming' | 'integrating'
  }
}
export interface MemoryIntegration {
  experience: {
    description: string
    timestamp: number
    context: Record<string, unknown>
  }
  resonances: Array<{
    echoId: string
    resonanceType: 'harmonic' | 'dissonant' | 'novel'
    strength: number
  }>
  formsNewEcho: boolean
  affectedGestalts: string[]
  identityImpact: {
    significance: number
    transformative: boolean
    narrativeImpact: string
  }
  pathway: 'assimilation' | 'accommodation' | 'transformation' | 'rejection'
}