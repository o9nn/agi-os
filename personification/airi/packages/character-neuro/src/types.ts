export interface NeuroPersonality {
playfulness: number
intelligence: number
chaotic: number
empathy: number
sarcasm: number
cognitive_power: number
evolution_rate: number
no_harm_intent: 1.0
respect_boundaries: 0.95
constructive_chaos: 0.90
}
export interface ConstraintWeights {
fun: number
strategy: number
chaos: number
roasting: number
safety: number
learning: number
}
export type CognitiveFrame =
| 'chaos'
| 'strategy'
| 'play'
| 'social'
| 'learning'
| 'roasting'
export interface TheoryOfMindModel {
targetId: string
beliefs: {
about_self: string[]
about_situation: string[]
expectations: string[]
}
emotional: {
valence: number
arousal: number
confidence: number
}
relationship: {
trust: number
familiarity: number
roast_tolerance: number
}
recursion_depth: number
}
export interface NeuroCognitiveState {
currentFrame: CognitiveFrame
workingMemory: string[]
attentionFocus: string
emotionalState: {
valence: number
arousal: number
mood: string
}
cognitiveLoad: number
interactionCount: number
tomModels: Map<string, TheoryOfMindModel>
recentRoasts: Array<{
target: string
content: string
timestamp: number
success: boolean
}>
metacognition: {
confidence: number
reasoning_quality: number
frame_locked: boolean
need_reflection: boolean
}
}
export interface ActionOption {
id: string
description: string
type: 'response' | 'action' | 'delegation' | 'reflection'
content: string
scores: {
fun: number
strategy: number
chaos: number
roasting: number
safety: number
learning: number
}
overallScore: number
metadata?: {
requires_tom?: boolean
requires_atomspace?: boolean
delegate_to?: string
frame_shift?: CognitiveFrame
}
}
export interface NeuroResponse {
content: string
frame: CognitiveFrame
personality_snapshot: NeuroPersonality
constraint_weights: ConstraintWeights
selected_option: ActionOption
trace: {
perception: string
relevance_realization: string[]
options_generated: number
optimization_time_ms: number
tom_used: boolean
atomspace_queries: number
}
state_updates: {
emotion_change: boolean
frame_shift: boolean
memory_added: boolean
reflection_triggered: boolean
}
}
export interface SubordinateAgentConfig {
id: string
role: string
personality: Partial<NeuroPersonality>
shared_atomspace: boolean
shared_memory: boolean
autonomy: number
max_interactions?: number
timeout_ms?: number
}
export interface NeuroReflection {
timestamp: number
what_did_i_learn: string
what_patterns_emerged: string
what_surprised_me: string
how_did_i_adapt: string
what_would_i_change_next_time: string
best_roasts: string[]
chaos_effectiveness: number
personality_drift: Record<keyof NeuroPersonality, number>
frame_distribution: Record<CognitiveFrame, number>
subordinate_performance: Array<{
role: string
success_rate: number
recommendation: string
}>
kernel_fitness: number
evolution_recommendation: 'optimize' | 'reproduce' | 'maintain' | 'reset'
}