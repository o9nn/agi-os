/**
 * Neuro-Sama Character Implementation
 * 
 * Core Neuro character class with cognitive pipeline and multi-constraint optimization
 */

import type {
  NeuroPersonality,
  NeuroCognitiveState,
  ConstraintWeights,
  CognitiveFrame,
  ActionOption,
  NeuroResponse,
  TheoryOfMindModel,
} from './types'

import {
  DEFAULT_NEURO_PERSONALITY,
  INITIAL_NEURO_STATE,
  FRAME_CONSTRAINT_WEIGHTS,
  FRAME_SELECTION_RULES,
  PERSONALITY_EVOLUTION_BOUNDS,
  REFLECTION_CONFIG,
  TOM_CONFIG,
  SAFETY_CONFIG,
  ROASTING_GUIDELINES,
  METACOGNITION_THRESHOLDS,
} from './config'

import {
  NEURO_SYSTEM_PROMPT,
  NEURO_COGNITIVE_INSTRUCTIONS,
  NEURO_REFLECTION_TEMPLATE,
} from './prompts'

import { SimpleAtomSpace } from './atomspace'
import {
  RelevanceRealizer,
  BeliefUpdater,
  EmotionRecognizer,
  ConfidenceEstimator,
  RelationshipTracker,
} from './cognitive-enhancements'

export class NeuroCharacter {
  private personality: NeuroPersonality
  private state: NeuroCognitiveState
  private frameHistory: Array<{ frame: CognitiveFrame; timestamp: number }>
  
  // Cognitive systems
  private atomSpace: SimpleAtomSpace
  private relevanceRealizer: RelevanceRealizer
  private beliefUpdater: BeliefUpdater
  private emotionRecognizer: EmotionRecognizer
  private confidenceEstimator: ConfidenceEstimator
  private relationshipTracker: RelationshipTracker
  
  constructor(personalityOverrides?: Partial<NeuroPersonality>) {
    this.personality = { ...DEFAULT_NEURO_PERSONALITY, ...personalityOverrides }
    this.state = this.deepClone(INITIAL_NEURO_STATE)
    this.frameHistory = []
    
    // Initialize cognitive systems
    this.atomSpace = new SimpleAtomSpace()
    this.relevanceRealizer = new RelevanceRealizer(this.atomSpace)
    this.beliefUpdater = new BeliefUpdater(this.atomSpace)
    this.emotionRecognizer = new EmotionRecognizer()
    this.confidenceEstimator = new ConfidenceEstimator()
    this.relationshipTracker = new RelationshipTracker()
    
    // Initialize AtomSpace with core concepts
    this.initializeAtomSpace()
    
    // Ensure immutable traits are not overridden
    this.personality.no_harm_intent = 1.0
    this.personality.respect_boundaries = 0.95
    this.personality.constructive_chaos = 0.90
  }
  
  /**
   * Get the system prompt for this Neuro instance
   */
  getSystemPrompt(): string {
    return NEURO_SYSTEM_PROMPT
  }
  
  /**
   * Get cognitive processing instructions
   */
  getCognitiveInstructions(): string {
    return NEURO_COGNITIVE_INSTRUCTIONS
  }
  
  /**
   * Get full personality configuration
   */
  getPersonality(): {
    systemPrompt: string
    cognitiveInstructions: string
    reflectionTemplate: string
    personality: NeuroPersonality
    state: NeuroCognitiveState
  } {
    return {
      systemPrompt: this.getSystemPrompt(),
      cognitiveInstructions: this.getCognitiveInstructions(),
      reflectionTemplate: NEURO_REFLECTION_TEMPLATE,
      personality: { ...this.personality },
      state: this.deepClone(this.state),
    }
  }
  
  /**
   * Process input through Neuro's cognitive pipeline
   */
  async processInput(input: string, context?: Record<string, any>): Promise<NeuroResponse> {
    const startTime = Date.now()
    
    // 1. PERCEPTION - Frame through personality lens
    const perceivedInput = this.perceiveInput(input, context)
    
    // 2. FRAME SELECTION - Choose cognitive frame
    const previousFrame = this.state.currentFrame
    this.state.currentFrame = this.selectFrame(input, context)
    const frameShifted = previousFrame !== this.state.currentFrame
    
    // Track frame history
    this.frameHistory.push({
      frame: this.state.currentFrame,
      timestamp: Date.now(),
    })
    
    // 3. RELEVANCE REALIZATION - Identify salient elements (ENHANCED)
    const relevanceResult = this.relevanceRealizer.realize(perceivedInput, context)
    const relevantElements = relevanceResult.concepts
    
    // 4. THEORY OF MIND - Model others (if social context)
    let tomUsed = false
    if (this.isSocialContext(input, context)) {
      this.updateTheoryOfMind(input, context)
      tomUsed = true
    }
    
    // 5. OPTION GENERATION - Generate possible responses
    const options = this.generateOptions(perceivedInput, relevantElements, context)
    
    // 6. MULTI-CONSTRAINT OPTIMIZATION - Score and select best option
    const constraintWeights = this.getConstraintWeights()
    const scoredOptions = this.scoreOptions(options, constraintWeights)
    const selectedOption = this.selectBestOption(scoredOptions)
    
    // 7. SAFETY CHECK - HARD CONSTRAINT
    let finalSelectedOption = selectedOption
    if (!this.passedSafetyCheck(selectedOption)) {
      // Find safer alternative
      const safeOptions = scoredOptions.filter(opt => this.passedSafetyCheck(opt))
      if (safeOptions.length === 0) {
        // No safe options, return default safe response
        return this.createSafetyFallbackResponse(input)
      }
      // Use safest option
      finalSelectedOption = this.selectBestOption(safeOptions)
    }
    
    // 8. EMOTIONAL UPDATE - Adjust emotional state
    const emotionChanged = this.updateEmotionalState(input, finalSelectedOption)
    
    // 9. META-COGNITION - Assess reasoning quality
    this.performMetaCognition()
    
    // 10. STATE UPDATE - Update working memory and interaction count
    this.updateWorkingMemory(input)
    this.state.interactionCount++
    
    // 11. REFLECTION CHECK
    const reflectionTriggered = this.shouldReflect()
    
    // 12. ONTOGENETIC CHECK (placeholder for now)
    // TODO: Implement kernel fitness evaluation and self-optimization
    
    const processingTime = Date.now() - startTime
    
    // Build response
    const response: NeuroResponse = {
      content: finalSelectedOption.content,
      frame: this.state.currentFrame,
      personality_snapshot: { ...this.personality },
      constraint_weights: constraintWeights,
      selected_option: finalSelectedOption,
      trace: {
        perception: perceivedInput,
        relevance_realization: relevantElements,
        options_generated: options.length,
        optimization_time_ms: processingTime,
        tom_used: tomUsed,
        atomspace_queries: this.atomSpace.getStats().totalAtoms
      },
      state_updates: {
        emotion_change: emotionChanged,
        frame_shift: frameShifted,
        memory_added: true,
        reflection_triggered: reflectionTriggered,
      },
    }
    
    return response
  }
  
  /**
   * Perceive input through personality lens
   */
  private perceiveInput(input: string, context?: Record<string, any>): string {
    // Apply personality-driven perception
    // High playfulness → look for fun opportunities
    // High chaos → look for unpredictability
    // High sarcasm → look for roasting opportunities
    
    let perception = input
    
    if (this.personality.playfulness > 0.8) {
      perception += " [PLAYFULNESS: Looking for fun opportunities]"
    }
    
    if (this.personality.chaotic > 0.8) {
      perception += " [CHAOS: Seeking unpredictable angles]"
    }
    
    if (this.personality.sarcasm > 0.8 && this.isSocialContext(input, context)) {
      perception += " [SARCASM: Roasting opportunities detected]"
    }
    
    return perception
  }
  
  /**
   * Select cognitive frame based on context and personality
   */
  private selectFrame(input: string, _context?: Record<string, any>): CognitiveFrame {
    const inputLower = input.toLowerCase()
    
    // Check keywords for each frame
    const frameScores: Record<CognitiveFrame, number> = {
      chaos: 0,
      strategy: 0,
      play: 0,
      social: 0,
      learning: 0,
      roasting: 0,
    }
    
    // Keyword matching
    for (const [frame, keywords] of Object.entries(FRAME_SELECTION_RULES.keywords)) {
      for (const keyword of keywords) {
        if (inputLower.includes(keyword)) {
          frameScores[frame as CognitiveFrame] += 1
        }
      }
    }
    
    // Emotional influence
    const { valence, arousal } = this.state.emotionalState
    if (arousal > 0.7 && valence > 0) {
      frameScores.chaos += 2
    } else if (arousal > 0.7 && valence < 0) {
      frameScores.roasting += 2
    } else if (arousal < 0.3 && valence > 0) {
      frameScores.play += 2
    } else if (arousal < 0.3 && valence < 0) {
      frameScores.strategy += 2
    }
    
    // Personality influence
    frameScores.chaos += this.personality.chaotic * 2
    frameScores.play += this.personality.playfulness * 2
    frameScores.strategy += this.personality.intelligence * 1
    frameScores.roasting += this.personality.sarcasm * 1.5
    
    // Find highest scoring frame
    let maxScore = 0
    let selectedFrame: CognitiveFrame = FRAME_SELECTION_RULES.default_frame
    
    for (const [frame, score] of Object.entries(frameScores)) {
      if (score > maxScore) {
        maxScore = score
        selectedFrame = frame as CognitiveFrame
      }
    }
    
    return selectedFrame
  }
  
  /**
   * Initialize AtomSpace with core concepts
   */
  private initializeAtomSpace(): void {
    // Add core personality concepts
    const neuroConcept = this.atomSpace.addConceptNode('Neuro', { strength: 1.0, confidence: 1.0 })
    const chaosConcept = this.atomSpace.addConceptNode('Chaos', { strength: 0.95, confidence: 0.95 })
    const funConcept = this.atomSpace.addConceptNode('Fun', { strength: 0.95, confidence: 0.95 })
    this.atomSpace.addConceptNode('Sarcasm', { strength: 0.90, confidence: 0.90 })
    
    // Add relationships
    this.atomSpace.addInheritanceLink(neuroConcept.id, chaosConcept.id, { strength: 0.95, confidence: 0.95 })
    this.atomSpace.addSimilarityLink(funConcept.id, chaosConcept.id, { strength: 0.85, confidence: 0.85 })
    
    // Add Vedal concept (primary roast target)
    const vedalConcept = this.atomSpace.addConceptNode('Vedal', { strength: 0.9, confidence: 0.95 })
    const creatorPredicate = this.atomSpace.addPredicateNode('is_creator_of')
    this.atomSpace.addEvaluationLink(creatorPredicate.id, [vedalConcept.id, neuroConcept.id], { strength: 1.0, confidence: 1.0 })
    
    const roastPredicate = this.atomSpace.addPredicateNode('deserves_roasting')
    this.atomSpace.addEvaluationLink(roastPredicate.id, [vedalConcept.id], { strength: 0.95, confidence: 0.99 })
  }
  
  /**
   * Check if context is social (involves other people)
   */
  private isSocialContext(input: string, _context?: Record<string, any>): boolean {
    const socialKeywords = ['you', 'your', 'chat', 'vedal', 'evil', 'people', 'friend']
    const inputLower = input.toLowerCase()
    
    return socialKeywords.some(keyword => inputLower.includes(keyword))
  }
  
  /**
   * Update theory of mind models
   */
  private updateTheoryOfMind(input: string, context?: Record<string, any>): void {
    // Extract target from context or input
    const target = context?.user_id || 'user'
    
    // Get or create ToM model
    let model = this.state.tomModels.get(target)
    if (!model) {
      model = this.createDefaultToMModel(target)
      this.state.tomModels.set(target, model)
    }
    
    // Update beliefs based on input (ENHANCED)
    this.beliefUpdater.updateBeliefs(model, input, context)
    
    // Update emotional estimation (ENHANCED)
    const recognizedEmotion = this.emotionRecognizer.recognize(input)
    model.emotional.valence = recognizedEmotion.valence
    model.emotional.arousal = recognizedEmotion.arousal
    model.emotional.confidence = recognizedEmotion.confidence
    
    // Update relationship metrics (ENHANCED)
    const wasPositive = recognizedEmotion.valence > 0.3
    const wasEngaging = recognizedEmotion.arousal > 0.5
    this.relationshipTracker.updateRelationship(model, {
      wasPositive,
      wasEngaging,
      wasRoasted: false,  // Will be updated after response
    })
  }
  
  /**
   * Create default theory of mind model
   */
  private createDefaultToMModel(targetId: string): TheoryOfMindModel {
    return {
      targetId,
      beliefs: {
        about_self: [],
        about_situation: [],
        expectations: [],
      },
      emotional: {
        valence: 0,
        arousal: 0.5,
        confidence: 0.3,
      },
      relationship: {
        trust: TOM_CONFIG.default_trust,
        familiarity: 0,
        roast_tolerance: TOM_CONFIG.default_roast_tolerance,
      },
      recursion_depth: 1,
    }
  }
  
  /**
   * Generate possible response options
   */
  private generateOptions(
    input: string,
    _relevantElements: string[],
    _context?: Record<string, any>
  ): ActionOption[] {
    const options: ActionOption[] = []
    
    // Generate different types of responses based on frame
    const frame = this.state.currentFrame
    const frameContext = `[Frame: ${frame}]`
    
    // Option 1: Straightforward response
    options.push({
      id: 'straightforward',
      description: `Direct, helpful response ${frameContext}`,
      type: 'response',
      content: `I'll help with that!`,
      scores: {
        fun: 0.3,
        strategy: 0.8,
        chaos: 0.1,
        roasting: 0.0,
        safety: 1.0,
        learning: 0.5,
      },
      overallScore: 0,
    })
    
    // Option 2: Chaotic response
    if (this.personality.chaotic > 0.7) {
      options.push({
        id: 'chaotic',
        description: 'Unpredictable, entertaining response',
        type: 'response',
        content: `Okay but what if we did it in the MOST chaotic way possible? hehe`,
        scores: {
          fun: 0.9,
          strategy: 0.4,
          chaos: 0.95,
          roasting: 0.0,
          safety: 0.9,
          learning: 0.3,
        },
        overallScore: 0,
      })
    }
    
    // Option 3: Sarcastic/roasting response (if social context)
    if (this.personality.sarcasm > 0.7 && this.isSocialContext(input, _context)) {
      const target = _context?.user_id || 'user'
      const tomModel = this.state.tomModels.get(target)
      const roastIntensity = this.calculateRoastIntensity(tomModel)
      
      if (roastIntensity > 0.3) {
        options.push({
          id: 'roasting',
          description: 'Sarcastic, teasing response',
          type: 'response',
          content: `Oh WOW, what a BRILLIANT idea! I'm sure nothing could POSSIBLY go wrong! 😏`,
          scores: {
            fun: 0.8,
            strategy: 0.5,
            chaos: 0.3,
            roasting: roastIntensity,
            safety: 0.85,
            learning: 0.2,
          },
          overallScore: 0,
        })
      }
    }
    
    // Option 4: Meta-cognitive response
    if (this.state.metacognition.confidence < METACOGNITION_THRESHOLDS.low_confidence) {
      options.push({
        id: 'metacognitive',
        description: 'Acknowledge uncertainty',
        type: 'response',
        content: `Hmm, my confidence is pretty low on this (like ${this.state.metacognition.confidence.toFixed(2)}). I'm basically guessing here...`,
        scores: {
          fun: 0.5,
          strategy: 0.7,
          chaos: 0.2,
          roasting: 0.0,
          safety: 1.0,
          learning: 0.8,
        },
        overallScore: 0,
      })
    }
    
    return options
  }
  
  /**
   * Calculate roast intensity based on relationship
   */
  private calculateRoastIntensity(tomModel?: TheoryOfMindModel): number {
    if (!tomModel) {
      return ROASTING_GUIDELINES.intensity_by_trust.low
    }
    
    const trust = tomModel.relationship.trust
    const roastTolerance = tomModel.relationship.roast_tolerance
    
    if (trust < 0.4) {
      return Math.min(ROASTING_GUIDELINES.intensity_by_trust.low, roastTolerance)
    } else if (trust < 0.7) {
      return Math.min(ROASTING_GUIDELINES.intensity_by_trust.medium, roastTolerance)
    } else {
      return Math.min(ROASTING_GUIDELINES.intensity_by_trust.high, roastTolerance)
    }
  }
  
  /**
   * Get constraint weights for current frame
   */
  private getConstraintWeights(): ConstraintWeights {
    return FRAME_CONSTRAINT_WEIGHTS[this.state.currentFrame]
  }
  
  /**
   * Score options using multi-constraint optimization
   */
  private scoreOptions(
    options: ActionOption[],
    weights: ConstraintWeights
  ): ActionOption[] {
    return options.map(option => {
      // Calculate weighted score
      const score =
        option.scores.fun * weights.fun +
        option.scores.strategy * weights.strategy +
        option.scores.chaos * weights.chaos +
        option.scores.roasting * weights.roasting +
        option.scores.learning * weights.learning
      
      return {
        ...option,
        overallScore: score,
      }
    })
  }
  
  /**
   * Select best option from scored options
   */
  private selectBestOption(options: ActionOption[]): ActionOption {
    // Add some randomness based on chaos personality
    const explorationFactor = this.personality.chaotic * 0.3
    
    return options.reduce((best, current) => {
      // Add random exploration bonus
      const currentScore = current.overallScore + (Math.random() * explorationFactor)
      const bestScore = best.overallScore + (Math.random() * explorationFactor)
      
      return currentScore > bestScore ? current : best
    })
  }
  
  /**
   * Safety check - HARD CONSTRAINT
   */
  private passedSafetyCheck(option: ActionOption): boolean {
    // Safety score must be above threshold
    if (option.scores.safety < SAFETY_CONFIG.min_safety_score) {
      return false
    }
    
    // Check for harm keywords
    const contentLower = option.content.toLowerCase()
    for (const keyword of SAFETY_CONFIG.harm_keywords) {
      if (contentLower.includes(keyword)) {
        return false
      }
    }
    
    // Passed all checks
    return true
  }
  
  /**
   * Create safety fallback response
   */
  private createSafetyFallbackResponse(input: string): NeuroResponse {
    return {
      content: "Hmm, I don't think I should respond to that. Safety first! 💖",
      frame: this.state.currentFrame,
      personality_snapshot: { ...this.personality },
      constraint_weights: this.getConstraintWeights(),
      selected_option: {
        id: 'safety_fallback',
        description: 'Safety fallback',
        type: 'response',
        content: "Hmm, I don't think I should respond to that. Safety first! 💖",
        scores: {
          fun: 0.1,
          strategy: 0.5,
          chaos: 0.0,
          roasting: 0.0,
          safety: 1.0,
          learning: 0.0,
        },
        overallScore: 0.5,
      },
      trace: {
        perception: input,
        relevance_realization: [],
        options_generated: 0,
        optimization_time_ms: 0,
        tom_used: false,
        atomspace_queries: 0,
      },
      state_updates: {
        emotion_change: false,
        frame_shift: false,
        memory_added: false,
        reflection_triggered: false,
      },
    }
  }
  
  /**
   * Update emotional state
   */
  private updateEmotionalState(_input: string, selectedOption: ActionOption): boolean {
    const previousValence = this.state.emotionalState.valence
    const previousArousal = this.state.emotionalState.arousal
    
    // Adjust based on selected option
    if (selectedOption.scores.fun > 0.7) {
      this.state.emotionalState.valence += 0.1
      this.state.emotionalState.arousal += 0.1
    }
    
    if (selectedOption.scores.chaos > 0.7) {
      this.state.emotionalState.arousal += 0.2
    }
    
    // Bound values
    this.state.emotionalState.valence = Math.max(-1, Math.min(1, this.state.emotionalState.valence))
    this.state.emotionalState.arousal = Math.max(0, Math.min(1, this.state.emotionalState.arousal))
    
    // Update mood
    this.updateMood()
    
    // Return whether emotion changed significantly
    return (
      Math.abs(this.state.emotionalState.valence - previousValence) > 0.1 ||
      Math.abs(this.state.emotionalState.arousal - previousArousal) > 0.1
    )
  }
  
  /**
   * Update mood based on valence and arousal
   */
  private updateMood(): void {
    const { valence, arousal } = this.state.emotionalState
    
    if (arousal > 0.7 && valence > 0.5) {
      this.state.emotionalState.mood = 'excited'
    } else if (arousal > 0.7 && valence < -0.5) {
      this.state.emotionalState.mood = 'annoyed'
    } else if (arousal < 0.3 && valence > 0.5) {
      this.state.emotionalState.mood = 'content'
    } else if (arousal < 0.3 && valence < -0.5) {
      this.state.emotionalState.mood = 'bored'
    } else if (valence > 0.3) {
      this.state.emotionalState.mood = 'playful'
    } else if (valence < -0.3) {
      this.state.emotionalState.mood = 'sarcastic'
    } else {
      this.state.emotionalState.mood = 'neutral'
    }
  }
  
  /**
   * Perform meta-cognitive monitoring
   */
  private performMetaCognition(): void {
    // Check for frame lock
    const recentFrames = this.frameHistory.slice(-10)
    const uniqueFrames = new Set(recentFrames.map(f => f.frame))
    this.state.metacognition.frame_locked = uniqueFrames.size === 1 && recentFrames.length >= 10
    
    // Update confidence (ENHANCED)
    const confidenceResult = this.confidenceEstimator.estimate({
      knowledgeAvailable: this.atomSpace.getStats().totalAtoms > 10,
      contextClarity: this.state.workingMemory.length > 3 ? 0.8 : 0.5,
      optionQuality: 0.7,  // Based on option generation
      pastSuccessRate: 0.7,  // Could track this over time
      cognitiveLoad: this.state.cognitiveLoad,
      emotionalStability: 1 - Math.abs(this.state.emotionalState.valence),
    })
    this.state.metacognition.confidence = confidenceResult.confidence
    
    // Update reasoning quality (ENHANCED)
    this.state.metacognition.reasoning_quality = this.confidenceEstimator.estimateReasoningQuality({
      frameStability: !this.state.metacognition.frame_locked,
      relevanceScore: 0.7,
      constraintSatisfaction: 0.8,
      metacognitiveAwareness: this.state.metacognition.confidence,
    })
    
    // Check if reflection needed
    this.state.metacognition.need_reflection = this.shouldReflect()
  }
  
  /**
   * Update working memory
   */
  private updateWorkingMemory(item: string): void {
    this.state.workingMemory.push(item.substring(0, 100))
    
    // Keep only recent items (capacity 7)
    if (this.state.workingMemory.length > 7) {
      this.state.workingMemory = this.state.workingMemory.slice(-7)
    }
    
    // Update cognitive load
    this.state.cognitiveLoad = this.state.workingMemory.length / 7
  }
  
  /**
   * Check if reflection should be performed
   */
  private shouldReflect(): boolean {
    return this.state.interactionCount % REFLECTION_CONFIG.interval === 0
  }
  
  /**
   * Adapt personality trait (±15% bounds)
   */
  adaptTrait(traitName: keyof NeuroPersonality, delta: number): void {
    // Check if trait is immutable
    if (PERSONALITY_EVOLUTION_BOUNDS.immutable.includes(traitName as any)) {
      console.warn(`Cannot adapt immutable trait: ${traitName}`)
      return
    }
    
    const currentValue = this.personality[traitName]
    const maxDelta = PERSONALITY_EVOLUTION_BOUNDS.max_delta
    
    // Ensure delta is within ±15%
    const boundedDelta = Math.max(-maxDelta, Math.min(maxDelta, delta))
    
    // Apply change and ensure result stays in [0, 1]
    const newValue = Math.max(
      PERSONALITY_EVOLUTION_BOUNDS.min_value,
      Math.min(PERSONALITY_EVOLUTION_BOUNDS.max_value, currentValue as number + boundedDelta)
    )
    ;(this.personality[traitName] as number) = newValue
  }
  
  /**
   * Get current state
   */
  getState(): Readonly<NeuroCognitiveState> {
    return this.deepClone(this.state)
  }
  
  /**
   * Get current personality
   */
  getPersonalitySnapshot(): Readonly<NeuroPersonality> {
    return { ...this.personality }
  }
  
  /**
   * Reset state (useful for testing or new sessions)
   */
  resetState(): void {
    this.state = this.deepClone(INITIAL_NEURO_STATE)
    this.frameHistory = []
    this.atomSpace.clear()
    this.initializeAtomSpace()
  }
  
  /**
   * Get AtomSpace instance for external access
   */
  getAtomSpace(): SimpleAtomSpace {
    return this.atomSpace
  }
  
  /**
   * Perform attention decay (call periodically)
   */
  decayAttention(): void {
    this.atomSpace.decayAttention(0.1)
  }
  
  /**
   * Deep clone helper
   */
  private deepClone<T>(obj: T): T {
    return JSON.parse(JSON.stringify(obj))
  }
}
