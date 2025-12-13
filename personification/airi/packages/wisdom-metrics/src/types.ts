/**
 * Wisdom Metrics - Types
 * 
 * Type definitions for wisdom cultivation tracking
 * Based on integration of morality, meaning, and mastery
 */

import type { FourWaysOfKnowing } from '@proj-airi/cognitive-core'

/**
 * Morality Metrics
 * 
 * Virtue, ethics, compassion, care for others
 */
export interface MoralityMetrics {
  /** Overall morality score (0-1) */
  overall: number
  
  /** Empathy demonstrated in interactions */
  empathy: number
  
  /** Consistency between values and actions */
  ethicalConsistency: number
  
  /** Compassionate actions count */
  compassionateActions: number
  
  /** Harm prevention / reduction */
  harmReduction: number
  
  /** Justice and fairness considerations */
  justice: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Meaning Metrics
 * 
 * Narrative coherence, purpose, significance, existential engagement
 */
export interface MeaningMetrics {
  /** Overall meaning score (0-1) */
  overall: number
  
  /** Coherence of life narrative */
  narrativeCoherence: number
  
  /** Stability of identity over time */
  identityStability: number
  
  /** Engagement with existential questions */
  existentialEngagement: number
  
  /** Sense of purpose and direction */
  purposeClarity: number
  
  /** Connection to something larger than self */
  transcendence: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Mastery Metrics
 * 
 * Skill development, competence, effectiveness
 */
export interface MasteryMetrics {
  /** Overall mastery score (0-1) */
  overall: number
  
  /** Problem-solving effectiveness */
  problemSolving: number
  
  /** Adaptability to new situations */
  adaptability: number
  
  /** Skill progression across domains */
  skillProgression: Map<string, number>
  
  /** Average skill level */
  averageSkillLevel: number
  
  /** Learning velocity (rate of improvement) */
  learningVelocity: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Integration Metrics
 * 
 * How well the three aspects work together
 */
export interface IntegrationMetrics {
  /** Balance among morality, meaning, mastery */
  threeAspectsBalance: number
  
  /** Four ways of knowing balance */
  fourWaysBalance: FourWaysOfKnowing
  
  /** Relevance realization quality */
  relevanceRealization: number
  
  /** Sophrosyne (optimal self-regulation) */
  sophrosyne: number
  
  /** Truth orientation (seeking truth over comfort) */
  truthOrientation: number
  
  /** Active open-mindedness */
  openMindedness: number
  
  /** Timestamp */
  timestamp: number
}

/**
 * Comprehensive Wisdom Assessment
 * 
 * All wisdom metrics integrated
 */
export interface WisdomMetrics {
  /** Overall wisdom score (0-1) */
  overall: number
  
  /** Morality aspect */
  morality: MoralityMetrics
  
  /** Meaning aspect */
  meaning: MeaningMetrics
  
  /** Mastery aspect */
  mastery: MasteryMetrics
  
  /** Integration quality */
  integration: IntegrationMetrics
  
  /** Agent identifier */
  agentId: string
  
  /** Timestamp */
  timestamp: number
  
  /** Time period assessed */
  period?: {
    start: number
    end: number
  }
}

/**
 * Wisdom Event
 * 
 * Something that contributes to or detracts from wisdom
 */
export interface WisdomEvent {
  /** Event identifier */
  id: string
  
  /** Agent */
  agentId: string
  
  /** Type of event */
  type: 'moral' | 'meaningful' | 'mastery' | 'integration'
  
  /** Description */
  description: string
  
  /** Impact on wisdom (positive or negative) */
  impact: {
    morality?: number
    meaning?: number
    mastery?: number
  }
  
  /** Context */
  context?: Record<string, unknown>
  
  /** Timestamp */
  timestamp: number
}

/**
 * Wisdom Recommendation
 * 
 * Suggestions for cultivating wisdom
 */
export interface WisdomRecommendation {
  /** Which aspect needs attention */
  aspect: 'morality' | 'meaning' | 'mastery' | 'integration'
  
  /** Current score */
  current: number
  
  /** Target score */
  target: number
  
  /** Gap to close */
  gap: number
  
  /** Specific recommendation */
  recommendation: string
  
  /** Suggested practices */
  practices: string[]
  
  /** Priority (0-1, higher = more urgent) */
  priority: number
  
  /** Estimated time to improvement */
  estimatedTime?: string
}

/**
 * Wisdom Progress
 * 
 * Historical wisdom development
 */
export interface WisdomProgress {
  /** Agent identifier */
  agentId: string
  
  /** Historical data points */
  history: Array<{
    timestamp: number
    wisdom: WisdomMetrics
  }>
  
  /** Trend analysis */
  trends: {
    morality: 'improving' | 'stable' | 'declining'
    meaning: 'improving' | 'stable' | 'declining'
    mastery: 'improving' | 'stable' | 'declining'
    overall: 'improving' | 'stable' | 'declining'
  }
  
  /** Growth rate (percentage per time unit) */
  growthRate: {
    morality: number
    meaning: number
    mastery: number
    overall: number
  }
}
