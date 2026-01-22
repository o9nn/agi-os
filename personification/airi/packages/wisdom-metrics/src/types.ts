import type { FourWaysOfKnowing } from '@proj-airi/cognitive-core'
export interface MoralityMetrics {
  overall: number
  empathy: number
  ethicalConsistency: number
  compassionateActions: number
  harmReduction: number
  justice: number
  timestamp: number
}
export interface MeaningMetrics {
  overall: number
  narrativeCoherence: number
  identityStability: number
  existentialEngagement: number
  purposeClarity: number
  transcendence: number
  timestamp: number
}
export interface MasteryMetrics {
  overall: number
  problemSolving: number
  adaptability: number
  skillProgression: Map<string, number>
  averageSkillLevel: number
  learningVelocity: number
  timestamp: number
}
export interface IntegrationMetrics {
  threeAspectsBalance: number
  fourWaysBalance: FourWaysOfKnowing
  relevanceRealization: number
  sophrosyne: number
  truthOrientation: number
  openMindedness: number
  timestamp: number
}
export interface WisdomMetrics {
  overall: number
  morality: MoralityMetrics
  meaning: MeaningMetrics
  mastery: MasteryMetrics
  integration: IntegrationMetrics
  agentId: string
  timestamp: number
  period?: {
    start: number
    end: number
  }
}
export interface WisdomEvent {
  id: string
  agentId: string
  type: 'moral' | 'meaningful' | 'mastery' | 'integration'
  description: string
  impact: {
    morality?: number
    meaning?: number
    mastery?: number
  }
  context?: Record<string, unknown>
  timestamp: number
}
export interface WisdomRecommendation {
  aspect: 'morality' | 'meaning' | 'mastery' | 'integration'
  current: number
  target: number
  gap: number
  recommendation: string
  practices: string[]
  priority: number
  estimatedTime?: string
}
export interface WisdomProgress {
  agentId: string
  history: Array<{
    timestamp: number
    wisdom: WisdomMetrics
  }>
  trends: {
    morality: 'improving' | 'stable' | 'declining'
    meaning: 'improving' | 'stable' | 'declining'
    mastery: 'improving' | 'stable' | 'declining'
    overall: 'improving' | 'stable' | 'declining'
  }
  growthRate: {
    morality: number
    meaning: number
    mastery: number
    overall: number
  }
}