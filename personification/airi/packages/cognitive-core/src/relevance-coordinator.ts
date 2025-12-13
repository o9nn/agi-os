/**
 * Relevance Coordinator
 * 
 * Core system for relevance realization - determining what matters
 * Based on John Vervaeke's cognitive science framework
 */

import type {
  CognitiveContext,
  Possibility,
  RelevanceScore,
  RankedPossibilities,
} from './types'

/**
 * Configuration for relevance calculation
 */
export interface RelevanceConfig {
  /** Weights for different relevance components */
  weights: {
    novelty: number
    emotional: number
    pragmatic: number
    coherence: number
    epistemic: number
  }
  
  /** Minimum relevance threshold */
  threshold: number
  
  /** Enable learning from outcomes */
  enableLearning: boolean
}

/**
 * Default configuration
 */
export const defaultRelevanceConfig: RelevanceConfig = {
  weights: {
    novelty: 0.2,
    emotional: 0.2,
    pragmatic: 0.3,
    coherence: 0.2,
    epistemic: 0.1,
  },
  threshold: 0.3,
  enableLearning: true,
}

/**
 * Relevance Coordinator
 * 
 * Implements systematic relevance realization for cognitive architecture
 */
export class RelevanceCoordinator {
  private config: RelevanceConfig
  private outcomeHistory: Array<{
    possibility: Possibility
    relevance: RelevanceScore
    outcome: 'success' | 'failure' | 'neutral'
    timestamp: number
  }> = []
  
  constructor(config: Partial<RelevanceConfig> = {}) {
    this.config = { ...defaultRelevanceConfig, ...config }
  }
  
  /**
   * Calculate relevance of a single possibility
   */
  async calculateRelevance(
    possibility: Possibility,
    context: CognitiveContext
  ): Promise<RelevanceScore> {
    const components = {
      novelty: this.assessNovelty(possibility, context),
      emotional: this.assessEmotionalResonance(possibility, context),
      pragmatic: this.assessPragmaticValue(possibility, context),
      coherence: this.assessCoherence(possibility, context),
      epistemic: this.assessEpistemicValue(possibility, context),
    }
    
    // Weighted sum
    const overall = Object.entries(components).reduce(
      (sum, [key, value]) => {
        const weight = this.config.weights[key as keyof typeof this.config.weights]
        return sum + (value * weight)
      },
      0
    )
    
    return {
      overall,
      components,
      confidence: 0.8, // TODO: Implement confidence calibration
      reasoning: this.generateReasoning(components, overall),
    }
  }
  
  /**
   * Rank multiple possibilities by relevance
   */
  async rankPossibilities(
    possibilities: Possibility[],
    context: CognitiveContext
  ): Promise<RankedPossibilities> {
    const scored = await Promise.all(
      possibilities.map(async possibility => ({
        possibility,
        relevance: await this.calculateRelevance(possibility, context),
      }))
    )
    
    // Sort by relevance (descending)
    const items = scored
      .filter(item => item.relevance.overall >= this.config.threshold)
      .sort((a, b) => b.relevance.overall - a.relevance.overall)
    
    return {
      items,
      context,
      timestamp: Date.now(),
    }
  }
  
  /**
   * Report outcome of acting on a possibility
   * Enables learning loop for relevance criteria
   */
  async reportOutcome(
    possibility: Possibility,
    relevance: RelevanceScore,
    outcome: 'success' | 'failure' | 'neutral'
  ): Promise<void> {
    this.outcomeHistory.push({
      possibility,
      relevance,
      outcome,
      timestamp: Date.now(),
    })
    
    if (this.config.enableLearning) {
      await this.updateWeights()
    }
  }
  
  /**
   * Assess novelty - how new/surprising is this?
   */
  private assessNovelty(
    possibility: Possibility,
    context: CognitiveContext
  ): number {
    // Check against working memory and recent history
    const inWorkingMemory = context.workingMemory.some(
      item => item.includes(possibility.description)
    )
    
    if (inWorkingMemory) {
      return 0.1 // Low novelty if in working memory
    }
    
    // Check recent history (if available)
    if (context.recentHistory) {
      const recentlySeen = context.recentHistory.some(
        item => JSON.stringify(item).includes(possibility.description)
      )
      
      if (recentlySeen) {
        return 0.3 // Medium-low novelty if recently seen
      }
    }
    
    return 0.8 // High novelty by default
  }
  
  /**
   * Assess emotional resonance
   */
  private assessEmotionalResonance(
    possibility: Possibility,
    context: CognitiveContext
  ): number {
    const { valence, arousal } = context.emotional
    
    // Higher arousal increases emotional weight of everything
    const arousalFactor = 0.5 + (arousal * 0.5)
    
    // Positive valence biases toward positive possibilities
    // Negative valence biases toward problem-solving possibilities
    const valenceBias = possibility.type === 'action' 
      ? Math.abs(valence) * 0.3
      : 0
    
    return (arousalFactor + valenceBias) / 1.3 // Normalize
  }
  
  /**
   * Assess pragmatic value - does this help achieve goals?
   */
  private assessPragmaticValue(
    possibility: Possibility,
    context: CognitiveContext
  ): number {
    // If we have a current task, assess contribution to it
    if (context.task) {
      // Simple heuristic: actions are more pragmatic during tasks
      if (possibility.type === 'action') {
        return 0.8
      }
      
      // Thoughts and memories are less immediately pragmatic
      if (possibility.type === 'thought' || possibility.type === 'memory') {
        return 0.3
      }
    }
    
    // Consider cost if available
    if (possibility.cost !== undefined) {
      // Lower cost = higher pragmatic value (inverse relationship)
      return Math.max(0, 1 - (possibility.cost / 10))
    }
    
    return 0.5 // Neutral default
  }
  
  /**
   * Assess coherence - does this fit with current understanding?
   */
  private assessCoherence(
    possibility: Possibility,
    context: CognitiveContext
  ): number {
    // Check alignment with attention focus
    if (context.attentionFocus) {
      const related = possibility.description
        .toLowerCase()
        .includes(context.attentionFocus.toLowerCase())
      
      if (related) {
        return 0.9
      }
    }
    
    // Check consistency with working memory
    const wmConsistency = context.workingMemory.length > 0
      ? 0.6
      : 0.4
    
    return wmConsistency
  }
  
  /**
   * Assess epistemic value - learning potential
   */
  private assessEpistemicValue(
    possibility: Possibility,
    context: CognitiveContext
  ): number {
    // Novelty contributes to epistemic value
    const novelty = this.assessNovelty(possibility, context)
    
    // Perceptions and thoughts have high epistemic value
    if (possibility.type === 'perception' || possibility.type === 'thought') {
      return Math.min(1, novelty + 0.3)
    }
    
    // Actions can lead to learning
    if (possibility.type === 'action') {
      return novelty * 0.7
    }
    
    return novelty * 0.5
  }
  
  /**
   * Generate human-readable reasoning
   */
  private generateReasoning(
    components: RelevanceScore['components'],
    overall: number
  ): string {
    const topFactors = Object.entries(components)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 2)
      .map(([key]) => key)
    
    return `Relevance (${overall.toFixed(2)}) primarily driven by ${topFactors.join(' and ')}`
  }
  
  /**
   * Update weights based on outcome history (learning loop)
   */
  private async updateWeights(): Promise<void> {
    if (this.outcomeHistory.length < 10) {
      return // Need enough data
    }
    
    // Simple learning: increase weights for components that predict success
    const recent = this.outcomeHistory.slice(-20)
    
    // This is a placeholder for more sophisticated learning
    // In production, would use proper statistical methods
    const successRate = recent.filter(h => h.outcome === 'success').length / recent.length
    
    // If success rate is low, slightly increase epistemic weight (explore more)
    if (successRate < 0.4) {
      this.config.weights.epistemic = Math.min(0.3, this.config.weights.epistemic * 1.1)
      this.config.weights.pragmatic = Math.max(0.1, this.config.weights.pragmatic * 0.9)
    }
    
    // If success rate is high, slightly increase pragmatic weight (exploit more)
    if (successRate > 0.7) {
      this.config.weights.pragmatic = Math.min(0.5, this.config.weights.pragmatic * 1.1)
      this.config.weights.epistemic = Math.max(0.05, this.config.weights.epistemic * 0.9)
    }
  }
  
  /**
   * Get current configuration
   */
  getConfig(): RelevanceConfig {
    return { ...this.config }
  }
  
  /**
   * Get outcome statistics
   */
  getStatistics(): {
    total: number
    successes: number
    failures: number
    successRate: number
  } {
    const total = this.outcomeHistory.length
    const successes = this.outcomeHistory.filter(h => h.outcome === 'success').length
    const failures = this.outcomeHistory.filter(h => h.outcome === 'failure').length
    
    return {
      total,
      successes,
      failures,
      successRate: total > 0 ? successes / total : 0,
    }
  }
}
