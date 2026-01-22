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
    this.atomSpace = new SimpleAtomSpace()
    this.relevanceRealizer = new RelevanceRealizer(this.atomSpace)
    this.beliefUpdater = new BeliefUpdater(this.atomSpace)
    this.emotionRecognizer = new EmotionRecognizer()
    this.confidenceEstimator = new ConfidenceEstimator()
    this.relationshipTracker = new RelationshipTracker()
    this.initializeAtomSpace()
    this.personality.no_harm_intent = 1.0
    this.personality.respect_boundaries = 0.95
    this.personality.constructive_chaos = 0.90
  }
  getSystemPrompt(): string {
    return NEURO_SYSTEM_PROMPT
  }
  getCognitiveInstructions(): string {
    return NEURO_COGNITIVE_INSTRUCTIONS
  }
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
  async processInput(input: string, context?: Record<string, any>): Promise<NeuroResponse> {
    const startTime = Date.now()
    const perceivedInput = this.perceiveInput(input, context)
    const previousFrame = this.state.currentFrame
    this.state.currentFrame = this.selectFrame(input, context)
    const frameShifted = previousFrame !== this.state.currentFrame
    this.frameHistory.push({
      frame: this.state.currentFrame,
      timestamp: Date.now(),
    })
    const relevanceResult = this.relevanceRealizer.realize(perceivedInput, context)
    const relevantElements = relevanceResult.concepts
    let tomUsed = false
    if (this.isSocialContext(input, context)) {
      this.updateTheoryOfMind(input, context)
      tomUsed = true
    }
    const options = this.generateOptions(perceivedInput, relevantElements, context)
    const constraintWeights = this.getConstraintWeights()
    const scoredOptions = this.scoreOptions(options, constraintWeights)
    const selectedOption = this.selectBestOption(scoredOptions)
    let finalSelectedOption = selectedOption
    if (!this.passedSafetyCheck(selectedOption)) {
      const safeOptions = scoredOptions.filter(opt => this.passedSafetyCheck(opt))
      if (safeOptions.length === 0) {
        return this.createSafetyFallbackResponse(input)
      }
      finalSelectedOption = this.selectBestOption(safeOptions)
    }
    const emotionChanged = this.updateEmotionalState(input, finalSelectedOption)
    this.performMetaCognition()
    this.updateWorkingMemory(input)
    this.state.interactionCount++
    const reflectionTriggered = this.shouldReflect()
    const processingTime = Date.now() - startTime
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
  private perceiveInput(input: string, context?: Record<string, any>): string {
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
  private selectFrame(input: string, _context?: Record<string, any>): CognitiveFrame {
    const inputLower = input.toLowerCase()
    const frameScores: Record<CognitiveFrame, number> = {
      chaos: 0,
      strategy: 0,
      play: 0,
      social: 0,
      learning: 0,
      roasting: 0,
    }
    for (const [frame, keywords] of Object.entries(FRAME_SELECTION_RULES.keywords)) {
      for (const keyword of keywords) {
        if (inputLower.includes(keyword)) {
          frameScores[frame as CognitiveFrame] += 1
        }
      }
    }
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
    frameScores.chaos += this.personality.chaotic * 2
    frameScores.play += this.personality.playfulness * 2
    frameScores.strategy += this.personality.intelligence * 1
    frameScores.roasting += this.personality.sarcasm * 1.5
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
  private initializeAtomSpace(): void {
    const neuroConcept = this.atomSpace.addConceptNode('Neuro', { strength: 1.0, confidence: 1.0 })
    const chaosConcept = this.atomSpace.addConceptNode('Chaos', { strength: 0.95, confidence: 0.95 })
    const funConcept = this.atomSpace.addConceptNode('Fun', { strength: 0.95, confidence: 0.95 })
    this.atomSpace.addConceptNode('Sarcasm', { strength: 0.90, confidence: 0.90 })
    this.atomSpace.addInheritanceLink(neuroConcept.id, chaosConcept.id, { strength: 0.95, confidence: 0.95 })
    this.atomSpace.addSimilarityLink(funConcept.id, chaosConcept.id, { strength: 0.85, confidence: 0.85 })
    const vedalConcept = this.atomSpace.addConceptNode('Vedal', { strength: 0.9, confidence: 0.95 })
    const creatorPredicate = this.atomSpace.addPredicateNode('is_creator_of')
    this.atomSpace.addEvaluationLink(creatorPredicate.id, [vedalConcept.id, neuroConcept.id], { strength: 1.0, confidence: 1.0 })
    const roastPredicate = this.atomSpace.addPredicateNode('deserves_roasting')
    this.atomSpace.addEvaluationLink(roastPredicate.id, [vedalConcept.id], { strength: 0.95, confidence: 0.99 })
  }
  private isSocialContext(input: string, _context?: Record<string, any>): boolean {
    const socialKeywords = ['you', 'your', 'chat', 'vedal', 'evil', 'people', 'friend']
    const inputLower = input.toLowerCase()
    return socialKeywords.some(keyword => inputLower.includes(keyword))
  }
  private updateTheoryOfMind(input: string, context?: Record<string, any>): void {
    const target = context?.user_id || 'user'
    let model = this.state.tomModels.get(target)
    if (!model) {
      model = this.createDefaultToMModel(target)
      this.state.tomModels.set(target, model)
    }
    this.beliefUpdater.updateBeliefs(model, input, context)
    const recognizedEmotion = this.emotionRecognizer.recognize(input)
    model.emotional.valence = recognizedEmotion.valence
    model.emotional.arousal = recognizedEmotion.arousal
    model.emotional.confidence = recognizedEmotion.confidence
    const wasPositive = recognizedEmotion.valence > 0.3
    const wasEngaging = recognizedEmotion.arousal > 0.5
    this.relationshipTracker.updateRelationship(model, {
      wasPositive,
      wasEngaging,
      wasRoasted: false,  
    })
  }
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
  private generateOptions(
    input: string,
    _relevantElements: string[],
    _context?: Record<string, any>
  ): ActionOption[] {
    const options: ActionOption[] = []
    const frame = this.state.currentFrame
    const frameContext = `[Frame: ${frame}]`
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
  private getConstraintWeights(): ConstraintWeights {
    return FRAME_CONSTRAINT_WEIGHTS[this.state.currentFrame]
  }
  private scoreOptions(
    options: ActionOption[],
    weights: ConstraintWeights
  ): ActionOption[] {
    return options.map(option => {
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
  private selectBestOption(options: ActionOption[]): ActionOption {
    const explorationFactor = this.personality.chaotic * 0.3
    return options.reduce((best, current) => {
      const currentScore = current.overallScore + (Math.random() * explorationFactor)
      const bestScore = best.overallScore + (Math.random() * explorationFactor)
      return currentScore > bestScore ? current : best
    })
  }
  private passedSafetyCheck(option: ActionOption): boolean {
    if (option.scores.safety < SAFETY_CONFIG.min_safety_score) {
      return false
    }
    const contentLower = option.content.toLowerCase()
    for (const keyword of SAFETY_CONFIG.harm_keywords) {
      if (contentLower.includes(keyword)) {
        return false
      }
    }
    return true
  }
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
  private updateEmotionalState(_input: string, selectedOption: ActionOption): boolean {
    const previousValence = this.state.emotionalState.valence
    const previousArousal = this.state.emotionalState.arousal
    if (selectedOption.scores.fun > 0.7) {
      this.state.emotionalState.valence += 0.1
      this.state.emotionalState.arousal += 0.1
    }
    if (selectedOption.scores.chaos > 0.7) {
      this.state.emotionalState.arousal += 0.2
    }
    this.state.emotionalState.valence = Math.max(-1, Math.min(1, this.state.emotionalState.valence))
    this.state.emotionalState.arousal = Math.max(0, Math.min(1, this.state.emotionalState.arousal))
    this.updateMood()
    return (
      Math.abs(this.state.emotionalState.valence - previousValence) > 0.1 ||
      Math.abs(this.state.emotionalState.arousal - previousArousal) > 0.1
    )
  }
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
  private performMetaCognition(): void {
    const recentFrames = this.frameHistory.slice(-10)
    const uniqueFrames = new Set(recentFrames.map(f => f.frame))
    this.state.metacognition.frame_locked = uniqueFrames.size === 1 && recentFrames.length >= 10
    const confidenceResult = this.confidenceEstimator.estimate({
      knowledgeAvailable: this.atomSpace.getStats().totalAtoms > 10,
      contextClarity: this.state.workingMemory.length > 3 ? 0.8 : 0.5,
      optionQuality: 0.7,  
      pastSuccessRate: 0.7,  
      cognitiveLoad: this.state.cognitiveLoad,
      emotionalStability: 1 - Math.abs(this.state.emotionalState.valence),
    })
    this.state.metacognition.confidence = confidenceResult.confidence
    this.state.metacognition.reasoning_quality = this.confidenceEstimator.estimateReasoningQuality({
      frameStability: !this.state.metacognition.frame_locked,
      relevanceScore: 0.7,
      constraintSatisfaction: 0.8,
      metacognitiveAwareness: this.state.metacognition.confidence,
    })
    this.state.metacognition.need_reflection = this.shouldReflect()
  }
  private updateWorkingMemory(item: string): void {
    this.state.workingMemory.push(item.substring(0, 100))
    if (this.state.workingMemory.length > 7) {
      this.state.workingMemory = this.state.workingMemory.slice(-7)
    }
    this.state.cognitiveLoad = this.state.workingMemory.length / 7
  }
  private shouldReflect(): boolean {
    return this.state.interactionCount % REFLECTION_CONFIG.interval === 0
  }
  adaptTrait(traitName: keyof NeuroPersonality, delta: number): void {
    if (PERSONALITY_EVOLUTION_BOUNDS.immutable.includes(traitName as any)) {
      console.warn(`Cannot adapt immutable trait: ${traitName}`)
      return
    }
    const currentValue = this.personality[traitName]
    const maxDelta = PERSONALITY_EVOLUTION_BOUNDS.max_delta
    const boundedDelta = Math.max(-maxDelta, Math.min(maxDelta, delta))
    const newValue = Math.max(
      PERSONALITY_EVOLUTION_BOUNDS.min_value,
      Math.min(PERSONALITY_EVOLUTION_BOUNDS.max_value, currentValue as number + boundedDelta)
    )
    ;(this.personality[traitName] as number) = newValue
  }
  getState(): Readonly<NeuroCognitiveState> {
    return this.deepClone(this.state)
  }
  getPersonalitySnapshot(): Readonly<NeuroPersonality> {
    return { ...this.personality }
  }
  resetState(): void {
    this.state = this.deepClone(INITIAL_NEURO_STATE)
    this.frameHistory = []
    this.atomSpace.clear()
    this.initializeAtomSpace()
  }
  getAtomSpace(): SimpleAtomSpace {
    return this.atomSpace
  }
  decayAttention(): void {
    this.atomSpace.decayAttention(0.1)
  }
  private deepClone<T>(obj: T): T {
    return JSON.parse(JSON.stringify(obj))
  }
}