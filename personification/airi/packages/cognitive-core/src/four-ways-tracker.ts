/**
 * Four Ways of Knowing Tracker
 * 
 * Tracks and balances the four ways of knowing:
 * - Propositional (knowing-that)
 * - Procedural (knowing-how)
 * - Perspectival (knowing-as)
 * - Participatory (knowing-by-being)
 */

import type { FourWaysOfKnowing } from './types'

/**
 * Event types that contribute to ways of knowing
 */
export type KnowingEvent = {
  type: 'propositional' | 'procedural' | 'perspectival' | 'participatory'
  description: string
  weight?: number // Relative importance (default: 1)
  timestamp: number
}

/**
 * Configuration for balance targets
 */
export interface BalanceConfig {
  /** Target percentages (should sum to 1.0) */
  targets: FourWaysOfKnowing
  
  /** Acceptable deviation from target */
  tolerance: number
  
  /** Time window for balance calculation (ms) */
  timeWindow: number
}

/**
 * Default balanced configuration (25% each ± 10%)
 */
export const defaultBalanceConfig: BalanceConfig = {
  targets: {
    propositional: 0.25,
    procedural: 0.25,
    perspectival: 0.25,
    participatory: 0.25,
  },
  tolerance: 0.10,
  timeWindow: 24 * 60 * 60 * 1000, // 24 hours
}

/**
 * Recommendations for balancing
 */
export interface BalanceRecommendation {
  /** Which way needs attention */
  way: keyof FourWaysOfKnowing
  
  /** Current percentage */
  current: number
  
  /** Target percentage */
  target: number
  
  /** Gap (positive = need more, negative = need less) */
  gap: number
  
  /** Specific recommendation */
  recommendation: string
  
  /** Priority (higher = more urgent) */
  priority: number
}

/**
 * Four Ways of Knowing Tracker
 */
export class FourWaysTracker {
  private events: KnowingEvent[] = []
  private config: BalanceConfig
  
  constructor(config: Partial<BalanceConfig> = {}) {
    this.config = { ...defaultBalanceConfig, ...config }
  }
  
  /**
   * Record a knowing event
   */
  recordEvent(event: Omit<KnowingEvent, 'timestamp'>): void {
    this.events.push({
      ...event,
      weight: event.weight ?? 1,
      timestamp: Date.now(),
    })
    
    // Keep only events within time window
    this.pruneOldEvents()
  }
  
  /**
   * Get current balance
   */
  getBalance(): FourWaysOfKnowing {
    this.pruneOldEvents()
    
    if (this.events.length === 0) {
      return {
        propositional: 0.25,
        procedural: 0.25,
        perspectival: 0.25,
        participatory: 0.25,
      }
    }
    
    const totals = {
      propositional: 0,
      procedural: 0,
      perspectival: 0,
      participatory: 0,
    }
    
    let totalWeight = 0
    
    for (const event of this.events) {
      const weight = event.weight ?? 1
      totals[event.type] += weight
      totalWeight += weight
    }
    
    return {
      propositional: totals.propositional / totalWeight,
      procedural: totals.procedural / totalWeight,
      perspectival: totals.perspectival / totalWeight,
      participatory: totals.participatory / totalWeight,
    }
  }
  
  /**
   * Check if balance is within tolerance
   */
  isBalanced(): boolean {
    const current = this.getBalance()
    const { targets, tolerance } = this.config
    
    return Object.entries(current).every(([key, value]) => {
      const target = targets[key as keyof FourWaysOfKnowing]
      return Math.abs(value - target) <= tolerance
    })
  }
  
  /**
   * Get recommendations for improving balance
   */
  getRecommendations(): BalanceRecommendation[] {
    const current = this.getBalance()
    const { targets } = this.config
    
    const recommendations: BalanceRecommendation[] = []
    
    // Check each way
    for (const [key, currentValue] of Object.entries(current)) {
      const way = key as keyof FourWaysOfKnowing
      const target = targets[way]
      const gap = target - currentValue
      
      // Only recommend if gap is significant
      if (Math.abs(gap) > this.config.tolerance / 2) {
        recommendations.push({
          way,
          current: currentValue,
          target,
          gap,
          recommendation: this.generateRecommendation(way, gap),
          priority: Math.abs(gap) / target, // Larger relative gap = higher priority
        })
      }
    }
    
    // Sort by priority (descending)
    return recommendations.sort((a, b) => b.priority - a.priority)
  }
  
  /**
   * Generate specific recommendation
   */
  private generateRecommendation(
    way: keyof FourWaysOfKnowing,
    gap: number
  ): string {
    const needMore = gap > 0
    
    const recommendations: Record<keyof FourWaysOfKnowing, { more: string; less: string }> = {
      propositional: {
        more: 'Engage in more fact-learning, reading, conceptual analysis, or theoretical reasoning',
        less: 'Reduce passive information consumption, focus on embodied practice instead',
      },
      procedural: {
        more: 'Practice skills, engage in hands-on activities, develop competencies through doing',
        less: 'Balance skill practice with reflection and conceptual understanding',
      },
      perspectival: {
        more: 'Explore different framings, practice reframing problems, shift attention patterns',
        less: 'Commit to perspectives longer, develop deeper understanding before shifting',
      },
      participatory: {
        more: 'Engage in identity-transforming experiences, deep relationships, existential reflection',
        less: 'Balance transformation with stability, integrate changes before seeking more',
      },
    }
    
    return needMore 
      ? recommendations[way].more
      : recommendations[way].less
  }
  
  /**
   * Remove events outside time window
   */
  private pruneOldEvents(): void {
    const cutoff = Date.now() - this.config.timeWindow
    this.events = this.events.filter(e => e.timestamp >= cutoff)
  }
  
  /**
   * Get statistics about recent activity
   */
  getStatistics(): {
    totalEvents: number
    eventsByType: Record<keyof FourWaysOfKnowing, number>
    balance: FourWaysOfKnowing
    isBalanced: boolean
    timeWindow: number
  } {
    this.pruneOldEvents()
    
    const eventsByType = {
      propositional: this.events.filter(e => e.type === 'propositional').length,
      procedural: this.events.filter(e => e.type === 'procedural').length,
      perspectival: this.events.filter(e => e.type === 'perspectival').length,
      participatory: this.events.filter(e => e.type === 'participatory').length,
    }
    
    return {
      totalEvents: this.events.length,
      eventsByType,
      balance: this.getBalance(),
      isBalanced: this.isBalanced(),
      timeWindow: this.config.timeWindow,
    }
  }
  
  /**
   * Reset tracker
   */
  reset(): void {
    this.events = []
  }
}
