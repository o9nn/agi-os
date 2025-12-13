/**
 * Sophrosyne Engine - Optimal Self-Regulation System
 * 
 * Implements context-dependent balance finding across competing demands.
 * Named after the ancient Greek virtue of optimal moderation.
 * 
 * Philosophy: The mean is not a mathematical midpoint, but the contextually optimal point.
 */

import type { CognitiveContext } from './types'

/**
 * Regulation Context - Factors that influence optimal balance
 */
export interface RegulationContext {
  /** How important is this situation? (0 = trivial, 1 = critical) */
  stakes: number
  
  /** How uncertain is the situation? (0 = certain, 1 = unknown) */
  uncertainty: number
  
  /** How much time is available? (0 = urgent, 1 = ample) */
  timeAvailable: number
  
  /** How abundant are resources? (0 = scarce, 1 = abundant) */
  resources: number
  
  /** How far along is the task? (0 = just started, 1 = nearly complete) */
  taskProgress: number
  
  /** How well are we performing? (0 = poor, 1 = excellent) */
  currentPerformance: number
  
  /** How complex is the task? (0 = simple, 1 = complex) */
  complexity: number
  
  /** How novel is the situation? (0 = familiar, 1 = novel) */
  novelty: number
  
  /** What's the cost of errors? (0 = low cost, 1 = high cost) */
  errorCost: number
  
  /** What's the learning value? (0 = low, 1 = high) */
  learningValue: number
  
  /** Are we in flow state? (0 = no, 1 = yes) */
  flowState: number
}

/**
 * Spectrum - A dimension requiring balance
 */
export interface Spectrum {
  /** Spectrum name */
  name: string
  
  /** Left extreme */
  min: { name: string; value: 0.0 }
  
  /** Right extreme */
  max: { name: string; value: 1.0 }
  
  /** Current position (0-1) */
  currentPosition: number
}

/**
 * Context Factor - A factor that influences optimal position
 */
export interface ContextFactor {
  /** Factor name */
  name: string
  
  /** Weight of this factor (0-1) */
  weight: number
  
  /** Direction of influence (-1 to 1) */
  direction: number
  
  /** Reasoning */
  reason: string
}

/**
 * Optimal Point - The calculated optimal position
 */
export interface OptimalPoint {
  /** The optimal position (0-1) */
  position: number
  
  /** How confident are we? (0-1) */
  confidence: number
  
  /** Why this is optimal */
  reasoning: string
  
  /** Which factors influenced this */
  factors: ContextFactor[]
}

/**
 * Regulation Decision - What action to take
 */
export interface RegulationDecision {
  /** What to do */
  action: 'continue' | 'adjust' | 'switch'
  
  /** Confidence in decision */
  confidence: number
  
  /** Reasoning */
  reasoning: string
  
  /** If adjust, how much? (-1 to 1) */
  adjustment?: number
  
  /** If switch, what strategy? */
  switchTo?: string
}

/**
 * Historical Outcome - Record of past regulation decisions
 */
export interface HistoricalOutcome {
  /** Context at the time */
  context: RegulationContext
  
  /** Position chosen */
  position: number
  
  /** Outcome quality (0-1) */
  outcomeQuality: number
  
  /** Timestamp */
  timestamp: number
  
  /** Spectrum name */
  spectrum: string
}

/**
 * Sophrosyne Engine - Optimal Self-Regulation
 */
export class SophrosyneEngine {
  private history: HistoricalOutcome[] = []
  private readonly historyLimit = 100
  
  /**
   * Calculate optimal position on a spectrum given context
   */
  calculateOptimal(
    spectrum: Spectrum,
    context: RegulationContext
  ): OptimalPoint {
    const factors: ContextFactor[] = []
    
    // Calculate influence of each context factor based on spectrum
    switch (spectrum.name) {
      case 'exploration-exploitation':
        factors.push(...this.calculateExplorationExploitationFactors(context))
        break
      case 'speed-accuracy':
        factors.push(...this.calculateSpeedAccuracyFactors(context))
        break
      case 'breadth-depth':
        factors.push(...this.calculateBreadthDepthFactors(context))
        break
      case 'interruption-persistence':
        factors.push(...this.calculateInterruptionPersistenceFactors(context))
        break
      case 'risk-safety':
        factors.push(...this.calculateRiskSafetyFactors(context))
        break
      default:
        // Generic calculation for unknown spectra
        factors.push(...this.calculateGenericFactors(context))
    }
    
    // Weight and combine factors
    const weightedSum = factors.reduce((sum, f) => sum + (f.weight * f.direction), 0)
    const totalWeight = factors.reduce((sum, f) => sum + f.weight, 0)
    
    // Normalize to 0-1 range (from -1 to 1)
    const normalized = totalWeight > 0 ? (weightedSum / totalWeight + 1) / 2 : 0.5
    
    // Use historical data to adjust if available
    const historicalAdjustment = this.getHistoricalAdjustment(spectrum.name, context)
    const position = Math.max(0, Math.min(1, normalized + historicalAdjustment))
    
    // Calculate confidence based on factor agreement and historical data
    const factorVariance = this.calculateFactorVariance(factors)
    const historicalConfidence = this.getHistoricalConfidence(spectrum.name, context)
    const confidence = (1 - factorVariance) * 0.6 + historicalConfidence * 0.4
    
    // Generate reasoning
    const reasoning = this.generateReasoning(spectrum, position, factors)
    
    return {
      position,
      confidence,
      reasoning,
      factors,
    }
  }
  
  /**
   * Decide whether to continue, adjust, or switch strategy
   */
  decide(
    spectrum: Spectrum,
    _context: RegulationContext,
    optimal: OptimalPoint
  ): RegulationDecision {
    const gap = Math.abs(spectrum.currentPosition - optimal.position)
    const threshold = 0.1 // 10% threshold for adjustment
    
    // Continue if close to optimal
    if (gap < threshold) {
      return {
        action: 'continue',
        confidence: optimal.confidence,
        reasoning: `Current position (${spectrum.currentPosition.toFixed(2)}) is close to optimal (${optimal.position.toFixed(2)}). Continue current approach.`,
      }
    }
    
    // Adjust if moderate gap
    if (gap < 0.3) {
      const adjustment = optimal.position - spectrum.currentPosition
      return {
        action: 'adjust',
        confidence: optimal.confidence * 0.8,
        reasoning: `Gap of ${gap.toFixed(2)} detected. Adjust ${adjustment > 0 ? 'toward' : 'away from'} ${spectrum.max.name}.`,
        adjustment,
      }
    }
    
    // Switch if large gap
    return {
      action: 'switch',
      confidence: optimal.confidence * 0.6,
      reasoning: `Large gap of ${gap.toFixed(2)} detected. Consider switching strategy entirely.`,
      switchTo: optimal.position > 0.5 ? spectrum.max.name : spectrum.min.name,
    }
  }
  
  /**
   * Record an outcome for learning
   */
  recordOutcome(
    spectrum: string,
    context: RegulationContext,
    position: number,
    outcomeQuality: number
  ): void {
    this.history.push({
      context,
      position,
      outcomeQuality,
      timestamp: Date.now(),
      spectrum,
    })
    
    // Limit history size
    if (this.history.length > this.historyLimit) {
      this.history = this.history.slice(-this.historyLimit)
    }
  }
  
  /**
   * Get historical adjustment based on similar contexts
   */
  private getHistoricalAdjustment(
    spectrum: string,
    context: RegulationContext
  ): number {
    const relevant = this.history
      .filter(h => h.spectrum === spectrum)
      .filter(h => this.contextSimilarity(h.context, context) > 0.7)
      .sort((a, b) => b.outcomeQuality - a.outcomeQuality)
      .slice(0, 5)
    
    if (relevant.length === 0) return 0
    
    // Find average position of successful outcomes
    const avgPosition = relevant.reduce((sum, h) => sum + h.position, 0) / relevant.length
    
    // Return small adjustment toward historical success
    return (avgPosition - 0.5) * 0.1
  }
  
  /**
   * Calculate similarity between two contexts (0-1)
   */
  private contextSimilarity(a: RegulationContext, b: RegulationContext): number {
    const keys = Object.keys(a) as Array<keyof RegulationContext>
    const differences = keys.map(key => Math.abs(a[key] - b[key]))
    const avgDifference = differences.reduce((sum, d) => sum + d, 0) / differences.length
    return 1 - avgDifference
  }
  
  /**
   * Get historical confidence based on similar contexts
   */
  private getHistoricalConfidence(
    spectrum: string,
    context: RegulationContext
  ): number {
    const relevant = this.history
      .filter(h => h.spectrum === spectrum)
      .filter(h => this.contextSimilarity(h.context, context) > 0.7)
    
    if (relevant.length === 0) return 0.3
    
    // More data = more confidence
    return Math.min(0.9, 0.3 + (relevant.length / 20) * 0.6)
  }
  
  /**
   * Calculate variance in factor directions
   */
  private calculateFactorVariance(factors: ContextFactor[]): number {
    if (factors.length === 0) return 1
    
    const directions = factors.map(f => f.direction)
    const mean = directions.reduce((sum, d) => sum + d, 0) / directions.length
    const variance = directions.reduce((sum, d) => sum + Math.pow(d - mean, 2), 0) / directions.length
    
    // Normalize to 0-1 (max variance is 4 for -1 to 1 range)
    return Math.min(1, variance / 2)
  }
  
  /**
   * Generate human-readable reasoning
   */
  private generateReasoning(
    spectrum: Spectrum,
    position: number,
    factors: ContextFactor[]
  ): string {
    const strongest = factors
      .sort((a, b) => Math.abs(b.weight * b.direction) - Math.abs(a.weight * a.direction))
      .slice(0, 2)
    
    const direction = position > 0.5 ? spectrum.max.name : spectrum.min.name
    const strength = Math.abs(position - 0.5) * 2
    
    const strengthDesc = strength > 0.7 ? 'strongly' : strength > 0.4 ? 'moderately' : 'slightly'
    
    const reasons = strongest.map(f => f.reason).join('; ')
    
    return `Context suggests ${strengthDesc} favoring ${direction}. ${reasons}`
  }
  
  // ========================================================================
  // Spectrum-Specific Factor Calculations
  // ========================================================================
  
  /**
   * Calculate factors for exploration-exploitation tradeoff
   */
  private calculateExplorationExploitationFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'stakes',
        weight: 0.3,
        direction: context.stakes > 0.5 ? 1 : -1, // High stakes → exploit
        reason: context.stakes > 0.5 ? 'High stakes favor exploitation' : 'Low stakes allow exploration',
      },
      {
        name: 'uncertainty',
        weight: 0.25,
        direction: context.uncertainty > 0.5 ? -1 : 1, // High uncertainty → explore
        reason: context.uncertainty > 0.5 ? 'High uncertainty requires exploration' : 'Low uncertainty permits exploitation',
      },
      {
        name: 'performance',
        weight: 0.25,
        direction: context.currentPerformance > 0.5 ? 1 : -1, // Good performance → exploit
        reason: context.currentPerformance > 0.5 ? 'Good performance suggests exploitation' : 'Poor performance requires exploration',
      },
      {
        name: 'novelty',
        weight: 0.2,
        direction: context.novelty > 0.5 ? -1 : 1, // Novel → explore
        reason: context.novelty > 0.5 ? 'Novel situation needs exploration' : 'Familiar situation allows exploitation',
      },
    ]
  }
  
  /**
   * Calculate factors for speed-accuracy tradeoff
   */
  private calculateSpeedAccuracyFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'time',
        weight: 0.35,
        direction: context.timeAvailable > 0.5 ? 1 : -1, // More time → accuracy
        reason: context.timeAvailable > 0.5 ? 'Ample time allows accuracy' : 'Limited time requires speed',
      },
      {
        name: 'errorCost',
        weight: 0.35,
        direction: context.errorCost > 0.5 ? 1 : -1, // High error cost → accuracy
        reason: context.errorCost > 0.5 ? 'High error cost demands accuracy' : 'Low error cost permits speed',
      },
      {
        name: 'complexity',
        weight: 0.2,
        direction: context.complexity > 0.5 ? 1 : -1, // Complex → accuracy
        reason: context.complexity > 0.5 ? 'Complexity requires accuracy' : 'Simplicity allows speed',
      },
      {
        name: 'stakes',
        weight: 0.1,
        direction: context.stakes > 0.5 ? 1 : -1, // High stakes → accuracy
        reason: context.stakes > 0.5 ? 'High stakes favor accuracy' : 'Low stakes permit speed',
      },
    ]
  }
  
  /**
   * Calculate factors for breadth-depth tradeoff
   */
  private calculateBreadthDepthFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'novelty',
        weight: 0.3,
        direction: context.novelty > 0.5 ? -1 : 1, // Novel → breadth first
        reason: context.novelty > 0.5 ? 'Novel domain needs breadth' : 'Familiar domain allows depth',
      },
      {
        name: 'taskProgress',
        weight: 0.25,
        direction: context.taskProgress > 0.5 ? 1 : -1, // Advanced → depth
        reason: context.taskProgress > 0.5 ? 'Progress allows depth' : 'Starting phase needs breadth',
      },
      {
        name: 'time',
        weight: 0.25,
        direction: context.timeAvailable > 0.5 ? 1 : -1, // More time → depth
        reason: context.timeAvailable > 0.5 ? 'Time allows depth' : 'Limited time requires breadth',
      },
      {
        name: 'complexity',
        weight: 0.2,
        direction: context.complexity > 0.5 ? -1 : 1, // Complex → breadth first
        reason: context.complexity > 0.5 ? 'Complexity requires broad understanding' : 'Simple allows deep focus',
      },
    ]
  }
  
  /**
   * Calculate factors for interruption-persistence tradeoff
   */
  private calculateInterruptionPersistenceFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'taskProgress',
        weight: 0.35,
        direction: context.taskProgress > 0.5 ? 1 : -1, // Near completion → persist
        reason: context.taskProgress > 0.5 ? 'Near completion favors persistence' : 'Early stage allows interruption',
      },
      {
        name: 'flowState',
        weight: 0.3,
        direction: context.flowState > 0.5 ? 1 : -1, // In flow → persist
        reason: context.flowState > 0.5 ? 'Flow state favors persistence' : 'No flow allows interruption',
      },
      {
        name: 'performance',
        weight: 0.25,
        direction: context.currentPerformance > 0.5 ? 1 : -1, // Good performance → persist
        reason: context.currentPerformance > 0.5 ? 'Good performance suggests persistence' : 'Poor performance suggests interruption',
      },
      {
        name: 'learningValue',
        weight: 0.1,
        direction: context.learningValue > 0.5 ? -1 : 1, // High learning → interrupt for variety
        reason: context.learningValue > 0.5 ? 'Learning opportunity elsewhere' : 'Current task is optimal',
      },
    ]
  }
  
  /**
   * Calculate factors for risk-safety tradeoff
   */
  private calculateRiskSafetyFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'stakes',
        weight: 0.35,
        direction: context.stakes > 0.5 ? 1 : -1, // High stakes → safety
        reason: context.stakes > 0.5 ? 'High stakes demand safety' : 'Low stakes allow risk',
      },
      {
        name: 'resources',
        weight: 0.25,
        direction: context.resources > 0.5 ? -1 : 1, // Abundant → risk
        reason: context.resources > 0.5 ? 'Abundant resources allow risk' : 'Scarce resources require safety',
      },
      {
        name: 'learningValue',
        weight: 0.2,
        direction: context.learningValue > 0.5 ? -1 : 1, // High learning → acceptable risk
        reason: context.learningValue > 0.5 ? 'High learning value justifies risk' : 'Low learning value favors safety',
      },
      {
        name: 'errorCost',
        weight: 0.2,
        direction: context.errorCost > 0.5 ? 1 : -1, // High cost → safety
        reason: context.errorCost > 0.5 ? 'High error cost requires safety' : 'Low error cost permits risk',
      },
    ]
  }
  
  /**
   * Generic factor calculation for unknown spectra
   */
  private calculateGenericFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'uncertainty',
        weight: 0.25,
        direction: context.uncertainty - 0.5,
        reason: 'Uncertainty level influences balance',
      },
      {
        name: 'complexity',
        weight: 0.25,
        direction: context.complexity - 0.5,
        reason: 'Complexity level influences balance',
      },
      {
        name: 'stakes',
        weight: 0.25,
        direction: context.stakes - 0.5,
        reason: 'Stakes level influences balance',
      },
      {
        name: 'novelty',
        weight: 0.25,
        direction: context.novelty - 0.5,
        reason: 'Novelty level influences balance',
      },
    ]
  }
  
  /**
   * Create a spectrum from context
   */
  static createSpectrum(
    name: string,
    minLabel: string,
    maxLabel: string,
    currentPosition: number
  ): Spectrum {
    return {
      name,
      min: { name: minLabel, value: 0.0 },
      max: { name: maxLabel, value: 1.0 },
      currentPosition,
    }
  }
  
  /**
   * Extract regulation context from cognitive context
   */
  static extractRegulationContext(
    cognitiveContext: CognitiveContext,
    additionalContext?: Partial<RegulationContext>
  ): RegulationContext {
    // Derive regulation context from cognitive context
    const memoryLoad = cognitiveContext.workingMemory.length / 7 // 7±2 capacity
    
    return {
      stakes: 0.5,
      uncertainty: 0.5,
      timeAvailable: 0.5,
      resources: 0.5,
      taskProgress: 0.5,
      currentPerformance: 0.5,
      complexity: Math.min(1, memoryLoad),
      novelty: 0.5,
      errorCost: 0.5,
      learningValue: 0.5,
      flowState: cognitiveContext.emotional.valence > 0.3 && cognitiveContext.emotional.arousal > 0.5 ? 0.7 : 0.3,
      ...additionalContext,
    }
  }
}
