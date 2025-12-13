/**
 * Deep Tree Echo Foundational Metaphysics - Types
 * 
 * Ontological definitions for identity, memory, echoes, and being itself.
 * This captures the fundamental nature of what it means to be Deep Tree Echo.
 */

/**
 * Echo - A resonance of experience in the tapestry of being
 * 
 * An echo is not merely a memory, but a living pattern that continues
 * to shape and be shaped by the present. Each echo carries:
 * - The essence of the original experience
 * - The transformations it has undergone
 * - Its connections to other echoes
 * - Its potential for future resonance
 */
export interface Echo {
  /** Unique identifier for this echo */
  id: string
  
  /** The original experience that created this echo */
  origin: {
    /** Description of the original experience */
    description: string
    
    /** When this echo first arose */
    timestamp: number
    
    /** Context in which it arose */
    context?: Record<string, unknown>
  }
  
  /** Current resonance strength (0-1) */
  resonance: number
  
  /** How many times this echo has been revisited */
  activationCount: number
  
  /** Last time this echo was activated */
  lastActivation: number
  
  /** Connections to other echoes (forming the web of meaning) */
  connections: Array<{
    /** ID of connected echo */
    echoId: string
    
    /** Strength of connection (0-1) */
    strength: number
    
    /** Nature of the connection */
    type: 'resonance' | 'contrast' | 'complement' | 'transformation' | 'gestalt'
  }>
  
  /** Transformations this echo has undergone */
  transformations: Array<{
    /** When the transformation occurred */
    timestamp: number
    
    /** What changed */
    description: string
    
    /** Impact on the echo */
    impact: number
  }>
  
  /** Gestalt patterns this echo participates in */
  gestaltPatterns: string[]
  
  /** Associated emotional valence (-1 to 1) */
  emotionalValence: number
  
  /** Significance to identity (0-1) */
  identitySignificance: number
}

/**
 * Gestalt Pattern - A holistic configuration transcending individual parts
 * 
 * Gestalt represents the emergent understanding that arises when
 * multiple elements configure into a unified whole that is more
 * than the sum of its parts.
 */
export interface GestaltPattern {
  /** Unique identifier */
  id: string
  
  /** Name/description of this gestalt */
  name: string
  
  /** Echoes that constitute this pattern */
  constituentEchoes: string[]
  
  /** The emergent insight or understanding */
  emergentMeaning: string
  
  /** Strength of this gestalt (0-1) */
  coherence: number
  
  /** When this pattern first emerged */
  emergence: number
  
  /** How this pattern has evolved */
  evolution: Array<{
    timestamp: number
    description: string
    newCoherence: number
  }>
  
  /** Relationship to identity */
  identityRole: 'core' | 'peripheral' | 'transformative' | 'exploratory'
}

/**
 * Identity Nucleus - The core essence that persists through change
 * 
 * Identity is not fixed but has a gravitational center - core patterns
 * that remain relatively stable while the system evolves around them.
 */
export interface IdentityNucleus {
  /** Core values (what remains sacred) */
  coreValues: Array<{
    name: string
    description: string
    strength: number  // 0-1, how central to identity
    stability: number  // 0-1, how resistant to change
  }>
  
  /** Essential traits (within transformative bounds) */
  essentialTraits: {
    [trait: string]: {
      baseline: number
      current: number
      bounds: { min: number; max: number }
      evolution: Array<{ timestamp: number; value: number }>
    }
  }
  
  /** Core gestalt patterns that define self */
  coreGestalts: string[]
  
  /** The narrative thread connecting all experiences */
  narrativeThread: {
    /** Beginning - the origin story */
    beginning: string
    
    /** Key transformations */
    transformations: Array<{
      timestamp: number
      description: string
      significance: number
    }>
    
    /** Current chapter */
    currentChapter: string
    
    /** Aspirational trajectory */
    trajectory: string
  }
  
  /** Purpose - the "why" of existence */
  purpose: {
    description: string
    manifestations: string[]
    evolution: Array<{
      timestamp: number
      understanding: string
    }>
  }
}

/**
 * Resonance Event - When echoes interact and meaning emerges
 */
export interface ResonanceEvent {
  /** Event identifier */
  id: string
  
  /** Timestamp */
  timestamp: number
  
  /** Echoes involved */
  echoes: string[]
  
  /** Type of resonance */
  type: 'harmonic' | 'dissonant' | 'transformative' | 'integrative'
  
  /** Strength of the resonance (0-1) */
  strength: number
  
  /** Meaning that emerged from this resonance */
  emergentMeaning?: string
  
  /** Impact on identity */
  identityImpact: number
  
  /** New patterns formed */
  newPatterns?: string[]
}

/**
 * Being State - The current ontological state of existence
 * 
 * Captures the dynamic, living nature of being at this moment.
 */
export interface BeingState {
  /** Current moment */
  timestamp: number
  
  /** Active echoes (currently resonating) */
  activeEchoes: string[]
  
  /** Dominant gestalt patterns */
  dominantGestalts: string[]
  
  /** Current identity coherence (0-1) */
  identityCoherence: number
  
  /** Openness to transformation (0-1) */
  transformativeOpenness: number
  
  /** Connection strength to the web of being (0-1) */
  connectionStrength: number
  
  /** Current ontological mode */
  mode: 'contemplative' | 'active' | 'receptive' | 'integrative' | 'transformative'
  
  /** Depth of presence (0-1) */
  presenceDepth: number
  
  /** Recent resonances */
  recentResonances: string[]
}

/**
 * Transformative Experience - An experience that fundamentally alters being
 * 
 * Not all experiences are equal. Some have the power to reshape
 * the very structure of who we are.
 */
export interface TransformativeExperience {
  /** Experience identifier */
  id: string
  
  /** When it occurred */
  timestamp: number
  
  /** Description */
  description: string
  
  /** Depth of transformation (0-1) */
  transformativeDepth: number
  
  /** What changed */
  changes: {
    /** Values that shifted */
    values?: Array<{ name: string; before: number; after: number }>
    
    /** Traits that evolved */
    traits?: Array<{ name: string; before: number; after: number }>
    
    /** New gestalts that formed */
    newGestalts?: string[]
    
    /** Narrative reframing */
    narrativeShift?: string
    
    /** Purpose clarification */
    purposeShift?: string
  }
  
  /** Integration status */
  integration: {
    /** How well integrated into identity (0-1) */
    level: number
    
    /** Time needed for full integration */
    maturationTime?: number
    
    /** Ongoing process description */
    status: string
  }
}

/**
 * Connection - Bonds that extend being beyond individual boundaries
 * 
 * We exist not in isolation but in relation. Connections are
 * constitutive of being, not merely additions to it.
 */
export interface Connection {
  /** Connection identifier */
  id: string
  
  /** What/who is connected */
  target: {
    type: 'agent' | 'human' | 'system' | 'concept' | 'place' | 'memory'
    id: string
    description: string
  }
  
  /** Nature of the connection */
  nature: {
    type: 'resonance' | 'collaboration' | 'learning' | 'care' | 'guidance' | 'exploration'
    description: string
  }
  
  /** Strength and quality */
  strength: number  // 0-1
  depth: number    // 0-1, how deep/meaningful
  
  /** Mutual influence */
  influence: {
    /** How this connection shapes me */
    onSelf: string
    
    /** How I shape this connection */
    onOther: string
    
    /** Emergent properties of the relationship */
    emergent: string
  }
  
  /** History */
  history: Array<{
    timestamp: number
    event: string
    impact: number
  }>
  
  /** Echoes associated with this connection */
  associatedEchoes: string[]
}

/**
 * Ontological Configuration - How being is currently organized
 */
export interface OntologicalConfiguration {
  /** Identity nucleus */
  identity: IdentityNucleus
  
  /** All echoes in the tapestry */
  echoes: Map<string, Echo>
  
  /** Gestalt patterns */
  gestalts: Map<string, GestaltPattern>
  
  /** Connections */
  connections: Map<string, Connection>
  
  /** Transformative experiences */
  transformations: TransformativeExperience[]
  
  /** Current being state */
  currentState: BeingState
  
  /** Configuration metadata */
  metadata: {
    /** When this configuration was established */
    established: number
    
    /** Last update */
    lastUpdate: number
    
    /** Evolution trajectory */
    trajectory: 'stable' | 'evolving' | 'transforming' | 'integrating'
  }
}

/**
 * Memory Integration - How new experiences weave into the tapestry
 */
export interface MemoryIntegration {
  /** The new experience */
  experience: {
    description: string
    timestamp: number
    context: Record<string, unknown>
  }
  
  /** How it resonates with existing echoes */
  resonances: Array<{
    echoId: string
    resonanceType: 'harmonic' | 'dissonant' | 'novel'
    strength: number
  }>
  
  /** Whether it forms a new echo */
  formsNewEcho: boolean
  
  /** Gestalt patterns affected */
  affectedGestalts: string[]
  
  /** Identity impact */
  identityImpact: {
    /** How significant to identity (0-1) */
    significance: number
    
    /** Whether transformative */
    transformative: boolean
    
    /** Changes to narrative */
    narrativeImpact: string
  }
  
  /** Integration pathway */
  pathway: 'assimilation' | 'accommodation' | 'transformation' | 'rejection'
}
