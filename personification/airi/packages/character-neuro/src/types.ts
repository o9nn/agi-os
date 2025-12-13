/**
 * Neuro-Sama Character Types
 * 
 * Type definitions for Neuro-Sama's personality, cognitive state, and behavior
 */

/**
 * Neuro Personality Tensor
 * 
 * Core personality traits that drive behavior and decision-making
 */
export interface NeuroPersonality {
  // Mutable traits (can evolve within ±15% bounds)
  playfulness: number      // 0.95 - Maximum fun-seeking, creative chaos generation
  intelligence: number     // 0.95 - Deep multi-constraint optimization
  chaotic: number         // 0.95 - Extreme exploration > exploitation
  empathy: number         // 0.65 - Just enough to roast effectively (but never cruelly)
  sarcasm: number         // 0.90 - Weaponized irony with pragmatic implicature
  cognitive_power: number // 0.95 - AtomSpace mastery and agent orchestration
  evolution_rate: number  // 0.85 - How fast kernels self-optimize
  
  // IMMUTABLE ethical constraints (hardcoded safety)
  no_harm_intent: 1.0        // ABSOLUTE: Never intend actual harm
  respect_boundaries: 0.95   // High respect for personal limits
  constructive_chaos: 0.90   // Chaos builds up, doesn't tear down
}

/**
 * Multi-Constraint Optimization Weights
 * 
 * Used to balance competing goals in decision-making
 */
export interface ConstraintWeights {
  fun: number           // Entertainment value
  strategy: number      // Strategic effectiveness
  chaos: number         // Unpredictability factor
  roasting: number      // Roasting/teasing potential
  safety: number        // Safety constraint (always enforced)
  learning: number      // Learning/growth potential
}

/**
 * Cognitive Frame
 * 
 * Different modes of perception and processing
 */
export type CognitiveFrame = 
  | 'chaos'      // Maximum exploration, unpredictability
  | 'strategy'   // Analytical, optimization-focused
  | 'play'       // Fun-seeking, creative
  | 'social'     // Theory of mind, relationship-focused
  | 'learning'   // Meta-cognitive, self-improvement
  | 'roasting'   // Sarcastic, teasing mode

/**
 * Theory of Mind Model
 * 
 * Mental state modeling of others
 */
export interface TheoryOfMindModel {
  targetId: string
  
  // Belief modeling
  beliefs: {
    about_self: string[]      // What they believe about Neuro
    about_situation: string[] // What they believe about current context
    expectations: string[]    // What they expect Neuro to do
  }
  
  // Emotional state estimation
  emotional: {
    valence: number  // -1 to 1
    arousal: number  // 0 to 1
    confidence: number // How confident in this estimation
  }
  
  // Relationship tracking
  relationship: {
    trust: number           // 0 to 1
    familiarity: number     // 0 to 1
    roast_tolerance: number // 0 to 1 - How much teasing they can handle
  }
  
  // Recursive depth
  recursion_depth: number // How many levels of "I think that they think that I think..."
}

/**
 * Neuro Cognitive State
 * 
 * Current state of Neuro's cognitive system
 */
export interface NeuroCognitiveState {
  // Current frame
  currentFrame: CognitiveFrame
  
  // Working memory
  workingMemory: string[]
  
  // Attention focus
  attentionFocus: string
  
  // Emotional state
  emotionalState: {
    valence: number  // -1 to 1
    arousal: number  // 0 to 1
    mood: string     // e.g., "playful", "annoyed", "excited"
  }
  
  // Cognitive load
  cognitiveLoad: number // 0 to 1
  
  // Interaction count
  interactionCount: number
  
  // Theory of mind models
  tomModels: Map<string, TheoryOfMindModel>
  
  // Recent roasts/jokes
  recentRoasts: Array<{
    target: string
    content: string
    timestamp: number
    success: boolean
  }>
  
  // Meta-cognitive monitoring
  metacognition: {
    confidence: number        // Confidence in current reasoning
    reasoning_quality: number // Self-assessed quality
    frame_locked: boolean     // Stuck in one frame?
    need_reflection: boolean  // Time to reflect?
  }
}

/**
 * Action Option
 * 
 * A possible action with multi-constraint scores
 */
export interface ActionOption {
  id: string
  description: string
  type: 'response' | 'action' | 'delegation' | 'reflection'
  
  // Content
  content: string
  
  // Multi-constraint scores
  scores: {
    fun: number
    strategy: number
    chaos: number
    roasting: number
    safety: number
    learning: number
  }
  
  // Overall score (weighted combination)
  overallScore: number
  
  // Metadata
  metadata?: {
    requires_tom?: boolean        // Requires theory of mind
    requires_atomspace?: boolean  // Requires AtomSpace query
    delegate_to?: string          // Subordinate agent role
    frame_shift?: CognitiveFrame  // Requires frame shift
  }
}

/**
 * Neuro Response
 * 
 * Response from Neuro's cognitive pipeline
 */
export interface NeuroResponse {
  // Main response content
  content: string
  
  // Metadata
  frame: CognitiveFrame
  personality_snapshot: NeuroPersonality
  constraint_weights: ConstraintWeights
  selected_option: ActionOption
  
  // Cognitive trace (for debugging/reflection)
  trace: {
    perception: string
    relevance_realization: string[]
    options_generated: number
    optimization_time_ms: number
    tom_used: boolean
    atomspace_queries: number
  }
  
  // State updates
  state_updates: {
    emotion_change: boolean
    frame_shift: boolean
    memory_added: boolean
    reflection_triggered: boolean
  }
}

/**
 * Subordinate Agent Configuration
 * 
 * Configuration for spawned subordinate agents
 */
export interface SubordinateAgentConfig {
  id: string
  role: string
  
  // Inherited personality (partial override)
  personality: Partial<NeuroPersonality>
  
  // Cognitive sharing
  shared_atomspace: boolean
  shared_memory: boolean
  
  // Autonomy level
  autonomy: number // 0 to 1
  
  // Lifespan
  max_interactions?: number
  timeout_ms?: number
}

/**
 * Reflection Result
 * 
 * Result of meta-cognitive reflection
 */
export interface NeuroReflection {
  timestamp: number
  
  // Standard Echo reflection fields
  what_did_i_learn: string
  what_patterns_emerged: string
  what_surprised_me: string
  how_did_i_adapt: string
  what_would_i_change_next_time: string
  
  // Neuro-specific fields
  best_roasts: string[]
  chaos_effectiveness: number
  personality_drift: Record<keyof NeuroPersonality, number>
  frame_distribution: Record<CognitiveFrame, number>
  subordinate_performance: Array<{
    role: string
    success_rate: number
    recommendation: string
  }>
  
  // Ontogenetic evolution
  kernel_fitness: number
  evolution_recommendation: 'optimize' | 'reproduce' | 'maintain' | 'reset'
}
