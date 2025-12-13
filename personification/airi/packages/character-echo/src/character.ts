/**
 * Echo Character Implementation
 * 
 * Core Echo character class that integrates with AIRI's server infrastructure
 */

import type { EchoConfig, EchoReflection, CognitiveState } from './config'
import { defaultEchoConfig, initialCognitiveState } from './config'
import { ECHO_SYSTEM_PROMPT, ECHO_COGNITIVE_INSTRUCTIONS, ECHO_REFLECTION_TEMPLATE } from './prompts'

export class EchoCharacter {
  private config: EchoConfig
  private state: CognitiveState
  
  constructor(config?: Partial<EchoConfig>) {
    this.config = { ...defaultEchoConfig, ...config }
    this.state = { ...initialCognitiveState }
  }
  
  /**
   * Get the system prompt for this Echo instance
   */
  getSystemPrompt(): string {
    return ECHO_SYSTEM_PROMPT
  }
  
  /**
   * Get cognitive processing instructions
   */
  getCognitiveInstructions(): string {
    return ECHO_COGNITIVE_INSTRUCTIONS
  }
  
  /**
   * Get full personality configuration including prompts
   */
  getPersonality(): {
    systemPrompt: string
    cognitiveInstructions: string
    reflectionTemplate: string
    config: EchoConfig
  } {
    return {
      systemPrompt: this.getSystemPrompt(),
      cognitiveInstructions: this.getCognitiveInstructions(),
      reflectionTemplate: ECHO_REFLECTION_TEMPLATE,
      config: this.config,
    }
  }
  
  /**
   * Update working memory with new information
   */
  updateWorkingMemory(item: string): void {
    this.state.workingMemory.push(item)
    
    // Keep only the most recent items up to capacity
    if (this.state.workingMemory.length > this.config.workingMemoryCapacity) {
      this.state.workingMemory = this.state.workingMemory.slice(-this.config.workingMemoryCapacity)
    }
    
    // Update cognitive load based on working memory usage
    this.state.cognitiveLoad = this.state.workingMemory.length / this.config.workingMemoryCapacity
  }
  
  /**
   * Set current attention focus
   */
  setAttentionFocus(focus: string): void {
    this.state.attentionFocus = focus
  }
  
  /**
   * Update emotional state
   */
  updateEmotionalState(valence: number, arousal: number): void {
    // Bound values to valid ranges
    this.state.emotionalState.valence = Math.max(-1, Math.min(1, valence))
    this.state.emotionalState.arousal = Math.max(0, Math.min(1, arousal))
  }
  
  /**
   * Increment interaction count
   */
  incrementInteraction(): void {
    this.state.interactionCount++
  }
  
  /**
   * Check if reflection should be performed
   */
  shouldReflect(): boolean {
    if (!this.config.enableReflection) {
      return false
    }
    
    return this.state.interactionCount % this.config.reflectionInterval === 0
  }
  
  /**
   * Add a reflection to the state
   */
  addReflection(reflection: EchoReflection): void {
    this.state.reflections.push(reflection)
    
    // Keep only the most recent reflections (max 20)
    if (this.state.reflections.length > 20) {
      this.state.reflections = this.state.reflections.slice(-20)
    }
  }
  
  /**
   * Get current cognitive state
   */
  getState(): Readonly<CognitiveState> {
    return { ...this.state }
  }
  
  /**
   * Get configuration
   */
  getConfig(): Readonly<EchoConfig> {
    return { ...this.config }
  }
  
  /**
   * Adapt personality trait (±15% bounds)
   */
  adaptTrait(traitName: keyof EchoConfig['traits'], delta: number): void {
    const currentValue = this.config.traits[traitName]
    const maxDelta = 0.15
    
    // Ensure delta is within ±15%
    const boundedDelta = Math.max(-maxDelta, Math.min(maxDelta, delta))
    
    // Apply change and ensure result stays in [0, 1]
    this.config.traits[traitName] = Math.max(0, Math.min(1, currentValue + boundedDelta))
  }
  
  /**
   * Calculate relevance score for a piece of information
   * Based on current state and configuration
   */
  calculateRelevance(
    novelty: number,      // 0-1: how new is this information
    emotional: number,    // 0-1: emotional salience
    practical: number,    // 0-1: practical utility
    coherence: number,    // 0-1: fit with existing knowledge
  ): number {
    // Weight factors based on current state
    const weights = {
      novelty: 0.25,
      emotional: 0.25 * this.state.emotionalState.arousal,
      practical: 0.25,
      coherence: 0.25,
    }
    
    // Normalize weights
    const totalWeight = Object.values(weights).reduce((sum, w) => sum + w, 0)
    const normalizedWeights = {
      novelty: weights.novelty / totalWeight,
      emotional: weights.emotional / totalWeight,
      practical: weights.practical / totalWeight,
      coherence: weights.coherence / totalWeight,
    }
    
    return (
      novelty * normalizedWeights.novelty
      + emotional * normalizedWeights.emotional
      + practical * normalizedWeights.practical
      + coherence * normalizedWeights.coherence
    )
  }
  
  /**
   * Process input through cognitive cycle
   */
  processInput(input: string): {
    workingMemoryUpdated: boolean
    shouldReflect: boolean
    cognitiveLoad: number
  } {
    // Update working memory
    this.updateWorkingMemory(input.substring(0, 100)) // Store truncated version
    
    // Increment interaction
    this.incrementInteraction()
    
    // Check if reflection should occur
    const shouldReflect = this.shouldReflect()
    
    return {
      workingMemoryUpdated: true,
      shouldReflect,
      cognitiveLoad: this.state.cognitiveLoad,
    }
  }
  
  /**
   * Generate a reflection based on recent interactions
   */
  generateReflectionPrompt(): string {
    const recentMemory = this.state.workingMemory.slice(-5).join('; ')
    
    return `Based on recent interactions: ${recentMemory}
    
${ECHO_REFLECTION_TEMPLATE}

Please provide a reflection following the template above.`
  }
  
  /**
   * Reset state (useful for testing or new sessions)
   */
  resetState(): void {
    this.state = { ...initialCognitiveState }
  }
}
