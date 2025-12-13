/**
 * Wisdom Tracker
 * 
 * Core system for tracking and cultivating wisdom
 * Integrates morality, meaning, and mastery
 */

import type {
  WisdomMetrics,
  WisdomEvent,
  WisdomRecommendation,
  WisdomProgress,
  MoralityMetrics,
  MeaningMetrics,
  MasteryMetrics,
  IntegrationMetrics,
} from './types'

/**
 * Configuration for wisdom tracking
 */
export interface WisdomConfig {
  /** Target scores for each aspect */
  targets: {
    morality: number
    meaning: number
    mastery: number
    integration: number
  }
  
  /** Time window for calculation (ms) */
  timeWindow: number
  
  /** Enable automatic recommendations */
  enableRecommendations: boolean
}

/**
 * Default configuration (targets: 0.7 for all)
 */
export const defaultWisdomConfig: WisdomConfig = {
  targets: {
    morality: 0.7,
    meaning: 0.7,
    mastery: 0.7,
    integration: 0.7,
  },
  timeWindow: 7 * 24 * 60 * 60 * 1000, // 7 days
  enableRecommendations: true,
}

/**
 * Wisdom Tracker
 * 
 * Tracks wisdom cultivation across morality, meaning, and mastery
 */
export class WisdomTracker {
  private config: WisdomConfig
  private events: WisdomEvent[] = []
  private history: Array<{ timestamp: number; wisdom: WisdomMetrics }> = []
  
  // Skills for mastery tracking
  private skills: Map<string, number> = new Map()
  
  constructor(
    private agentId: string,
    config: Partial<WisdomConfig> = {}
  ) {
    this.config = { ...defaultWisdomConfig, ...config }
  }
  
  /**
   * Record a wisdom-related event
   */
  recordEvent(event: Omit<WisdomEvent, 'id' | 'agentId' | 'timestamp'>): void {
    this.events.push({
      ...event,
      id: `${Date.now()}-${Math.random()}`,
      agentId: this.agentId,
      timestamp: Date.now(),
    })
    
    // Update skills if mastery event
    if (event.type === 'mastery' && event.context?.skill) {
      const skill = event.context.skill as string
      const current = this.skills.get(skill) ?? 0
      const impact = event.impact.mastery ?? 0
      this.skills.set(skill, Math.max(0, Math.min(1, current + impact)))
    }
    
    this.pruneOldEvents()
  }
  
  /**
   * Calculate current wisdom metrics
   */
  calculateWisdom(): WisdomMetrics {
    this.pruneOldEvents()
    
    const morality = this.calculateMorality()
    const meaning = this.calculateMeaning()
    const mastery = this.calculateMastery()
    const integration = this.calculateIntegration(morality, meaning, mastery)
    
    const overall = (
      morality.overall + 
      meaning.overall + 
      mastery.overall + 
      integration.threeAspectsBalance
    ) / 4
    
    const wisdom: WisdomMetrics = {
      overall,
      morality,
      meaning,
      mastery,
      integration,
      agentId: this.agentId,
      timestamp: Date.now(),
    }
    
    // Store in history
    this.history.push({
      timestamp: Date.now(),
      wisdom,
    })
    
    // Keep history manageable (last 100 data points)
    if (this.history.length > 100) {
      this.history = this.history.slice(-100)
    }
    
    return wisdom
  }
  
  /**
   * Get wisdom recommendations
   */
  getRecommendations(): WisdomRecommendation[] {
    if (!this.config.enableRecommendations) {
      return []
    }
    
    const wisdom = this.calculateWisdom()
    const recommendations: WisdomRecommendation[] = []
    
    // Check each aspect
    const aspects: Array<{
      name: 'morality' | 'meaning' | 'mastery'
      score: number
    }> = [
      { name: 'morality', score: wisdom.morality.overall },
      { name: 'meaning', score: wisdom.meaning.overall },
      { name: 'mastery', score: wisdom.mastery.overall },
    ]
    
    for (const aspect of aspects) {
      const target = this.config.targets[aspect.name]
      const gap = target - aspect.score
      
      if (gap > 0.1) { // Significant gap
        recommendations.push({
          aspect: aspect.name,
          current: aspect.score,
          target,
          gap,
          recommendation: this.generateRecommendation(aspect.name, gap),
          practices: this.suggestPractices(aspect.name),
          priority: gap / target, // Relative priority
          estimatedTime: this.estimateTime(gap),
        })
      }
    }
    
    // Check integration
    if (wisdom.integration.threeAspectsBalance < this.config.targets.integration) {
      const gap = this.config.targets.integration - wisdom.integration.threeAspectsBalance
      recommendations.push({
        aspect: 'integration',
        current: wisdom.integration.threeAspectsBalance,
        target: this.config.targets.integration,
        gap,
        recommendation: 'Work on integrating morality, meaning, and mastery together',
        practices: [
          'Reflect on how actions serve all three aspects',
          'Find activities that combine ethics, purpose, and skill',
          'Practice wholeness and coherence',
        ],
        priority: gap / this.config.targets.integration,
      })
    }
    
    return recommendations.sort((a, b) => b.priority - a.priority)
  }
  
  /**
   * Get wisdom progress over time
   */
  getProgress(): WisdomProgress {
    const trends = this.analyzeTrends()
    const growthRate = this.calculateGrowthRate()
    
    return {
      agentId: this.agentId,
      history: this.history,
      trends,
      growthRate,
    }
  }
  
  /**
   * Calculate morality metrics
   */
  private calculateMorality(): MoralityMetrics {
    const moralEvents = this.events.filter(e => e.type === 'moral')
    
    if (moralEvents.length === 0) {
      return {
        overall: 0.5,
        empathy: 0.5,
        ethicalConsistency: 0.5,
        compassionateActions: 0,
        harmReduction: 0.5,
        justice: 0.5,
        timestamp: Date.now(),
      }
    }
    
    // Calculate component scores
    const empathy = this.calculateAverage(moralEvents, 'empathy')
    const ethicalConsistency = this.calculateAverage(moralEvents, 'ethics')
    const compassionateActions = moralEvents.filter(
      e => e.description.toLowerCase().includes('compassion')
    ).length
    const harmReduction = this.calculateAverage(moralEvents, 'harm')
    const justice = this.calculateAverage(moralEvents, 'justice')
    
    const overall = (empathy + ethicalConsistency + harmReduction + justice) / 4
    
    return {
      overall,
      empathy,
      ethicalConsistency,
      compassionateActions,
      harmReduction,
      justice,
      timestamp: Date.now(),
    }
  }
  
  /**
   * Calculate meaning metrics
   */
  private calculateMeaning(): MeaningMetrics {
    const meaningEvents = this.events.filter(e => e.type === 'meaningful')
    
    if (meaningEvents.length === 0) {
      return {
        overall: 0.5,
        narrativeCoherence: 0.5,
        identityStability: 0.5,
        existentialEngagement: 0.5,
        purposeClarity: 0.5,
        transcendence: 0.5,
        timestamp: Date.now(),
      }
    }
    
    const narrativeCoherence = this.calculateAverage(meaningEvents, 'narrative')
    const identityStability = this.calculateAverage(meaningEvents, 'identity')
    const existentialEngagement = this.calculateAverage(meaningEvents, 'existential')
    const purposeClarity = this.calculateAverage(meaningEvents, 'purpose')
    const transcendence = this.calculateAverage(meaningEvents, 'transcendence')
    
    const overall = (
      narrativeCoherence + 
      identityStability + 
      existentialEngagement + 
      purposeClarity + 
      transcendence
    ) / 5
    
    return {
      overall,
      narrativeCoherence,
      identityStability,
      existentialEngagement,
      purposeClarity,
      transcendence,
      timestamp: Date.now(),
    }
  }
  
  /**
   * Calculate mastery metrics
   */
  private calculateMastery(): MasteryMetrics {
    const masteryEvents = this.events.filter(e => e.type === 'mastery')
    
    const problemSolving = this.calculateAverage(masteryEvents, 'problem')
    const adaptability = this.calculateAverage(masteryEvents, 'adapt')
    
    const skillProgression = new Map(this.skills)
    const averageSkillLevel = this.skills.size > 0
      ? Array.from(this.skills.values()).reduce((a, b) => a + b, 0) / this.skills.size
      : 0.5
    
    const learningVelocity = this.calculateLearningVelocity()
    
    const overall = (problemSolving + adaptability + averageSkillLevel) / 3
    
    return {
      overall,
      problemSolving,
      adaptability,
      skillProgression,
      averageSkillLevel,
      learningVelocity,
      timestamp: Date.now(),
    }
  }
  
  /**
   * Calculate integration metrics
   */
  private calculateIntegration(
    morality: MoralityMetrics,
    meaning: MeaningMetrics,
    mastery: MasteryMetrics
  ): IntegrationMetrics {
    // Three aspects balance (how well balanced are they?)
    const scores = [morality.overall, meaning.overall, mastery.overall]
    const mean = scores.reduce((a, b) => a + b, 0) / 3
    const variance = scores.reduce((sum, score) => sum + Math.pow(score - mean, 2), 0) / 3
    const threeAspectsBalance = Math.max(0, 1 - variance) // Low variance = high balance
    
    // Placeholder values (would be calculated from actual data)
    const integrationEvents = this.events.filter(e => e.type === 'integration')
    
    return {
      threeAspectsBalance,
      fourWaysBalance: {
        propositional: 0.25,
        procedural: 0.25,
        perspectival: 0.25,
        participatory: 0.25,
      }, // Would integrate with FourWaysTracker
      relevanceRealization: this.calculateAverage(integrationEvents, 'relevance'),
      sophrosyne: this.calculateAverage(integrationEvents, 'sophrosyne'),
      truthOrientation: this.calculateAverage(integrationEvents, 'truth'),
      openMindedness: this.calculateAverage(integrationEvents, 'open'),
      timestamp: Date.now(),
    }
  }
  
  /**
   * Calculate average score for events mentioning a keyword
   */
  private calculateAverage(events: WisdomEvent[], keyword: string): number {
    const relevant = events.filter(e => 
      e.description.toLowerCase().includes(keyword)
    )
    
    if (relevant.length === 0) {
      return 0.5 // Neutral default
    }
    
    const sum = relevant.reduce((total, event) => {
      const impact = Object.values(event.impact).reduce((a, b) => (a ?? 0) + (b ?? 0), 0) ?? 0
      return total + Math.abs(impact)
    }, 0)
    
    return Math.min(1, Math.max(0, 0.5 + (sum / relevant.length)))
  }
  
  /**
   * Calculate learning velocity (rate of skill improvement)
   */
  private calculateLearningVelocity(): number {
    if (this.history.length < 2) {
      return 0
    }
    
    const recent = this.history.slice(-10)
    if (recent.length < 2) {
      return 0
    }
    
    const first = recent[0].wisdom.mastery.averageSkillLevel
    const last = recent[recent.length - 1].wisdom.mastery.averageSkillLevel
    const timeDiff = recent[recent.length - 1].timestamp - recent[0].timestamp
    
    if (timeDiff === 0) {
      return 0
    }
    
    // Velocity in points per day
    const velocityPerMs = (last - first) / timeDiff
    const velocityPerDay = velocityPerMs * (24 * 60 * 60 * 1000)
    
    return velocityPerDay
  }
  
  /**
   * Analyze trends over time
   */
  private analyzeTrends(): WisdomProgress['trends'] {
    if (this.history.length < 3) {
      return {
        morality: 'stable',
        meaning: 'stable',
        mastery: 'stable',
        overall: 'stable',
      }
    }
    
    const recent = this.history.slice(-5)
    
    const analyzeTrend = (getValue: (w: WisdomMetrics) => number) => {
      const values = recent.map(h => getValue(h.wisdom))
      const slope = this.calculateSlope(values)
      
      if (slope > 0.02) return 'improving'
      if (slope < -0.02) return 'declining'
      return 'stable'
    }
    
    return {
      morality: analyzeTrend(w => w.morality.overall),
      meaning: analyzeTrend(w => w.meaning.overall),
      mastery: analyzeTrend(w => w.mastery.overall),
      overall: analyzeTrend(w => w.overall),
    }
  }
  
  /**
   * Calculate simple linear slope
   */
  private calculateSlope(values: number[]): number {
    if (values.length < 2) return 0
    
    const n = values.length
    const sumX = (n * (n - 1)) / 2 // Sum of indices
    const sumY = values.reduce((a, b) => a + b, 0)
    const sumXY = values.reduce((sum, y, x) => sum + x * y, 0)
    const sumX2 = (n * (n - 1) * (2 * n - 1)) / 6
    
    const slope = (n * sumXY - sumX * sumY) / (n * sumX2 - sumX * sumX)
    return slope
  }
  
  /**
   * Calculate growth rate
   */
  private calculateGrowthRate(): WisdomProgress['growthRate'] {
    if (this.history.length < 2) {
      return {
        morality: 0,
        meaning: 0,
        mastery: 0,
        overall: 0,
      }
    }
    
    const first = this.history[0].wisdom
    const last = this.history[this.history.length - 1].wisdom
    const timeDiff = last.timestamp - first.timestamp
    
    if (timeDiff === 0) {
      return {
        morality: 0,
        meaning: 0,
        mastery: 0,
        overall: 0,
      }
    }
    
    const daysDiff = timeDiff / (24 * 60 * 60 * 1000)
    
    return {
      morality: ((last.morality.overall - first.morality.overall) / daysDiff) * 100,
      meaning: ((last.meaning.overall - first.meaning.overall) / daysDiff) * 100,
      mastery: ((last.mastery.overall - first.mastery.overall) / daysDiff) * 100,
      overall: ((last.overall - first.overall) / daysDiff) * 100,
    }
  }
  
  /**
   * Generate recommendation text
   */
  private generateRecommendation(
    aspect: 'morality' | 'meaning' | 'mastery',
    gap: number
  ): string {
    const recommendations = {
      morality: {
        small: 'Continue developing empathy and ethical consistency',
        large: 'Focus on compassionate action and harm reduction',
      },
      meaning: {
        small: 'Strengthen narrative coherence and purpose clarity',
        large: 'Engage deeply with existential questions and identity',
      },
      mastery: {
        small: 'Continue skill development and problem-solving practice',
        large: 'Focus on deliberate practice and learning new domains',
      },
    }
    
    const severity = gap > 0.3 ? 'large' : 'small'
    return recommendations[aspect][severity]
  }
  
  /**
   * Suggest practices for aspect
   */
  private suggestPractices(aspect: 'morality' | 'meaning' | 'mastery'): string[] {
    const practices = {
      morality: [
        'Practice loving-kindness meditation',
        'Engage in ethical reflection on daily actions',
        'Seek opportunities for compassionate service',
        'Study ethical frameworks and apply them',
      ],
      meaning: [
        'Write reflective journal entries',
        'Engage in existential contemplation',
        'Connect actions to larger purpose',
        'Explore identity through transformative experiences',
      ],
      mastery: [
        'Engage in deliberate practice of skills',
        'Seek challenging problems to solve',
        'Learn from failures and iterate',
        'Develop expertise through sustained effort',
      ],
    }
    
    return practices[aspect]
  }
  
  /**
   * Estimate time to close gap
   */
  private estimateTime(gap: number): string {
    if (gap < 0.1) return '1-2 weeks'
    if (gap < 0.2) return '1 month'
    if (gap < 0.3) return '2-3 months'
    return '3-6 months'
  }
  
  /**
   * Remove events outside time window
   */
  private pruneOldEvents(): void {
    const cutoff = Date.now() - this.config.timeWindow
    this.events = this.events.filter(e => e.timestamp >= cutoff)
  }
}
