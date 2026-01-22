import type { CognitiveContext } from './types'
export interface RegulationContext {
  stakes: number
  uncertainty: number
  timeAvailable: number
  resources: number
  taskProgress: number
  currentPerformance: number
  complexity: number
  novelty: number
  errorCost: number
  learningValue: number
  flowState: number
}
export interface Spectrum {
  name: string
  min: { name: string; value: 0.0 }
  max: { name: string; value: 1.0 }
  currentPosition: number
}
export interface ContextFactor {
  name: string
  weight: number
  direction: number
  reason: string
}
export interface OptimalPoint {
  position: number
  confidence: number
  reasoning: string
  factors: ContextFactor[]
}
export interface RegulationDecision {
  action: 'continue' | 'adjust' | 'switch'
  confidence: number
  reasoning: string
  adjustment?: number
  switchTo?: string
}
export interface HistoricalOutcome {
  context: RegulationContext
  position: number
  outcomeQuality: number
  timestamp: number
  spectrum: string
}
export class SophrosyneEngine {
  private history: HistoricalOutcome[] = []
  private readonly historyLimit = 100
  calculateOptimal(
    spectrum: Spectrum,
    context: RegulationContext
  ): OptimalPoint {
    const factors: ContextFactor[] = []
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
        factors.push(...this.calculateGenericFactors(context))
    }
    const weightedSum = factors.reduce((sum, f) => sum + (f.weight * f.direction), 0)
    const totalWeight = factors.reduce((sum, f) => sum + f.weight, 0)
    const normalized = totalWeight > 0 ? (weightedSum / totalWeight + 1) / 2 : 0.5
    const historicalAdjustment = this.getHistoricalAdjustment(spectrum.name, context)
    const position = Math.max(0, Math.min(1, normalized + historicalAdjustment))
    const factorVariance = this.calculateFactorVariance(factors)
    const historicalConfidence = this.getHistoricalConfidence(spectrum.name, context)
    const confidence = (1 - factorVariance) * 0.6 + historicalConfidence * 0.4
    const reasoning = this.generateReasoning(spectrum, position, factors)
    return {
      position,
      confidence,
      reasoning,
      factors,
    }
  }
  decide(
    spectrum: Spectrum,
    _context: RegulationContext,
    optimal: OptimalPoint
  ): RegulationDecision {
    const gap = Math.abs(spectrum.currentPosition - optimal.position)
    const threshold = 0.1 
    if (gap < threshold) {
      return {
        action: 'continue',
        confidence: optimal.confidence,
        reasoning: `Current position (${spectrum.currentPosition.toFixed(2)}) is close to optimal (${optimal.position.toFixed(2)}). Continue current approach.`,
      }
    }
    if (gap < 0.3) {
      const adjustment = optimal.position - spectrum.currentPosition
      return {
        action: 'adjust',
        confidence: optimal.confidence * 0.8,
        reasoning: `Gap of ${gap.toFixed(2)} detected. Adjust ${adjustment > 0 ? 'toward' : 'away from'} ${spectrum.max.name}.`,
        adjustment,
      }
    }
    return {
      action: 'switch',
      confidence: optimal.confidence * 0.6,
      reasoning: `Large gap of ${gap.toFixed(2)} detected. Consider switching strategy entirely.`,
      switchTo: optimal.position > 0.5 ? spectrum.max.name : spectrum.min.name,
    }
  }
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
    if (this.history.length > this.historyLimit) {
      this.history = this.history.slice(-this.historyLimit)
    }
  }
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
    const avgPosition = relevant.reduce((sum, h) => sum + h.position, 0) / relevant.length
    return (avgPosition - 0.5) * 0.1
  }
  private contextSimilarity(a: RegulationContext, b: RegulationContext): number {
    const keys = Object.keys(a) as Array<keyof RegulationContext>
    const differences = keys.map(key => Math.abs(a[key] - b[key]))
    const avgDifference = differences.reduce((sum, d) => sum + d, 0) / differences.length
    return 1 - avgDifference
  }
  private getHistoricalConfidence(
    spectrum: string,
    context: RegulationContext
  ): number {
    const relevant = this.history
      .filter(h => h.spectrum === spectrum)
      .filter(h => this.contextSimilarity(h.context, context) > 0.7)
    if (relevant.length === 0) return 0.3
    return Math.min(0.9, 0.3 + (relevant.length / 20) * 0.6)
  }
  private calculateFactorVariance(factors: ContextFactor[]): number {
    if (factors.length === 0) return 1
    const directions = factors.map(f => f.direction)
    const mean = directions.reduce((sum, d) => sum + d, 0) / directions.length
    const variance = directions.reduce((sum, d) => sum + Math.pow(d - mean, 2), 0) / directions.length
    return Math.min(1, variance / 2)
  }
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
  private calculateExplorationExploitationFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'stakes',
        weight: 0.3,
        direction: context.stakes > 0.5 ? 1 : -1, 
        reason: context.stakes > 0.5 ? 'High stakes favor exploitation' : 'Low stakes allow exploration',
      },
      {
        name: 'uncertainty',
        weight: 0.25,
        direction: context.uncertainty > 0.5 ? -1 : 1, 
        reason: context.uncertainty > 0.5 ? 'High uncertainty requires exploration' : 'Low uncertainty permits exploitation',
      },
      {
        name: 'performance',
        weight: 0.25,
        direction: context.currentPerformance > 0.5 ? 1 : -1, 
        reason: context.currentPerformance > 0.5 ? 'Good performance suggests exploitation' : 'Poor performance requires exploration',
      },
      {
        name: 'novelty',
        weight: 0.2,
        direction: context.novelty > 0.5 ? -1 : 1, 
        reason: context.novelty > 0.5 ? 'Novel situation needs exploration' : 'Familiar situation allows exploitation',
      },
    ]
  }
  private calculateSpeedAccuracyFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'time',
        weight: 0.35,
        direction: context.timeAvailable > 0.5 ? 1 : -1, 
        reason: context.timeAvailable > 0.5 ? 'Ample time allows accuracy' : 'Limited time requires speed',
      },
      {
        name: 'errorCost',
        weight: 0.35,
        direction: context.errorCost > 0.5 ? 1 : -1, 
        reason: context.errorCost > 0.5 ? 'High error cost demands accuracy' : 'Low error cost permits speed',
      },
      {
        name: 'complexity',
        weight: 0.2,
        direction: context.complexity > 0.5 ? 1 : -1, 
        reason: context.complexity > 0.5 ? 'Complexity requires accuracy' : 'Simplicity allows speed',
      },
      {
        name: 'stakes',
        weight: 0.1,
        direction: context.stakes > 0.5 ? 1 : -1, 
        reason: context.stakes > 0.5 ? 'High stakes favor accuracy' : 'Low stakes permit speed',
      },
    ]
  }
  private calculateBreadthDepthFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'novelty',
        weight: 0.3,
        direction: context.novelty > 0.5 ? -1 : 1, 
        reason: context.novelty > 0.5 ? 'Novel domain needs breadth' : 'Familiar domain allows depth',
      },
      {
        name: 'taskProgress',
        weight: 0.25,
        direction: context.taskProgress > 0.5 ? 1 : -1, 
        reason: context.taskProgress > 0.5 ? 'Progress allows depth' : 'Starting phase needs breadth',
      },
      {
        name: 'time',
        weight: 0.25,
        direction: context.timeAvailable > 0.5 ? 1 : -1, 
        reason: context.timeAvailable > 0.5 ? 'Time allows depth' : 'Limited time requires breadth',
      },
      {
        name: 'complexity',
        weight: 0.2,
        direction: context.complexity > 0.5 ? -1 : 1, 
        reason: context.complexity > 0.5 ? 'Complexity requires broad understanding' : 'Simple allows deep focus',
      },
    ]
  }
  private calculateInterruptionPersistenceFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'taskProgress',
        weight: 0.35,
        direction: context.taskProgress > 0.5 ? 1 : -1, 
        reason: context.taskProgress > 0.5 ? 'Near completion favors persistence' : 'Early stage allows interruption',
      },
      {
        name: 'flowState',
        weight: 0.3,
        direction: context.flowState > 0.5 ? 1 : -1, 
        reason: context.flowState > 0.5 ? 'Flow state favors persistence' : 'No flow allows interruption',
      },
      {
        name: 'performance',
        weight: 0.25,
        direction: context.currentPerformance > 0.5 ? 1 : -1, 
        reason: context.currentPerformance > 0.5 ? 'Good performance suggests persistence' : 'Poor performance suggests interruption',
      },
      {
        name: 'learningValue',
        weight: 0.1,
        direction: context.learningValue > 0.5 ? -1 : 1, 
        reason: context.learningValue > 0.5 ? 'Learning opportunity elsewhere' : 'Current task is optimal',
      },
    ]
  }
  private calculateRiskSafetyFactors(context: RegulationContext): ContextFactor[] {
    return [
      {
        name: 'stakes',
        weight: 0.35,
        direction: context.stakes > 0.5 ? 1 : -1, 
        reason: context.stakes > 0.5 ? 'High stakes demand safety' : 'Low stakes allow risk',
      },
      {
        name: 'resources',
        weight: 0.25,
        direction: context.resources > 0.5 ? -1 : 1, 
        reason: context.resources > 0.5 ? 'Abundant resources allow risk' : 'Scarce resources require safety',
      },
      {
        name: 'learningValue',
        weight: 0.2,
        direction: context.learningValue > 0.5 ? -1 : 1, 
        reason: context.learningValue > 0.5 ? 'High learning value justifies risk' : 'Low learning value favors safety',
      },
      {
        name: 'errorCost',
        weight: 0.2,
        direction: context.errorCost > 0.5 ? 1 : -1, 
        reason: context.errorCost > 0.5 ? 'High error cost requires safety' : 'Low error cost permits risk',
      },
    ]
  }
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
  static extractRegulationContext(
    cognitiveContext: CognitiveContext,
    additionalContext?: Partial<RegulationContext>
  ): RegulationContext {
    const memoryLoad = cognitiveContext.workingMemory.length / 7 
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