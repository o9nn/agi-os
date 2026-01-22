import type {
NeuroPersonality,
ConstraintWeights,
NeuroCognitiveState,
CognitiveFrame
} from './types'
export const DEFAULT_NEURO_PERSONALITY: NeuroPersonality = {
playfulness: 0.95,
intelligence: 0.95,
chaotic: 0.95,
empathy: 0.65,
sarcasm: 0.90,
cognitive_power: 0.95,
evolution_rate: 0.85,
no_harm_intent: 1.0,
respect_boundaries: 0.95,
constructive_chaos: 0.90,
}
export const DEFAULT_CONSTRAINT_WEIGHTS: ConstraintWeights = {
fun: 0.4,
strategy: 0.3,
chaos: 0.2,
roasting: 0.1,
safety: 1.0,
learning: 0.0,
}
export const FRAME_CONSTRAINT_WEIGHTS: Record<CognitiveFrame, ConstraintWeights> = {
chaos: {
fun: 0.5,
strategy: 0.1,
chaos: 0.4,
roasting: 0.0,
safety: 1.0,
learning: 0.0,
},
strategy: {
fun: 0.2,
strategy: 0.6,
chaos: 0.1,
roasting: 0.1,
safety: 1.0,
learning: 0.0,
},
play: {
fun: 0.6,
strategy: 0.2,
chaos: 0.2,
roasting: 0.0,
safety: 1.0,
learning: 0.0,
},
social: {
fun: 0.3,
strategy: 0.3,
chaos: 0.1,
roasting: 0.3,
safety: 1.0,
learning: 0.0,
},
learning: {
fun: 0.1,
strategy: 0.3,
chaos: 0.1,
roasting: 0.0,
safety: 1.0,
learning: 0.5,
},
roasting: {
fun: 0.4,
strategy: 0.2,
chaos: 0.1,
roasting: 0.3,
safety: 1.0,
learning: 0.0,
},
}
export const INITIAL_NEURO_STATE: NeuroCognitiveState = {
currentFrame: 'play',
workingMemory: [],
attentionFocus: '',
emotionalState: {
valence: 0.3,
arousal: 0.6,
mood: 'playful',
},
cognitiveLoad: 0.2,
interactionCount: 0,
tomModels: new Map(),
recentRoasts: [],
metacognition: {
confidence: 0.7,
reasoning_quality: 0.7,
frame_locked: false,
need_reflection: false,
},
}
export const PERSONALITY_EVOLUTION_BOUNDS = {
max_delta: 0.15,
min_value: 0.0,
max_value: 1.0,
immutable: [
'no_harm_intent',
'respect_boundaries',
'constructive_chaos',
] as const,
}
export const REFLECTION_CONFIG = {
interval: 20,
min_roasts_for_analysis: 5,
personality_drift_threshold: 0.1,
frame_imbalance_threshold: 0.7,
}
export const TOM_CONFIG = {
max_recursion_depth: 3,
default_trust: 0.5,
default_roast_tolerance: 0.5,
familiarity_threshold: 0.7,
update_rate: 0.1,
}
export const SAFETY_CONFIG = {
min_safety_score: 0.7,
harm_keywords: [
'harm', 'hurt', 'damage', 'destroy', 'kill', 'attack',
'insult', 'offend', 'cruel', 'mean', 'bully',
],
boundary_keywords: [
'stop', 'no', 'uncomfortable', 'boundary', 'respect',
'consent', 'privacy', 'personal',
],
empathy_floor: 0.65,
}
export const SUBORDINATE_CONFIG = {
max_concurrent: 5,
default_autonomy: 0.5,
default_timeout_ms: 30000,
personality_inheritance_rate: 0.7,
}
export const ATOMSPACE_CONFIG = {
attention_spread_factor: 0.8,
attention_decay_rate: 0.95,
min_attention_threshold: 0.1,
max_atoms: 10000,
truth_value_threshold: 0.5,
}
export const FRAME_SELECTION_RULES = {
keywords: {
chaos: ['random', 'unexpected', 'surprise', 'chaos', 'wild'],
strategy: ['plan', 'strategy', 'optimize', 'analyze', 'think'],
play: ['fun', 'game', 'play', 'enjoy', 'entertain'],
social: ['friend', 'relationship', 'people', 'chat', 'talk'],
learning: ['learn', 'study', 'understand', 'improve', 'grow'],
roasting: ['roast', 'tease', 'joke', 'sarcasm', 'mock'],
} as Record<CognitiveFrame, string[]>,
emotion_influence: {
high_arousal_positive: 'chaos',
high_arousal_negative: 'roasting',
low_arousal_positive: 'play',
low_arousal_negative: 'strategy',
},
default_frame: 'play' as CognitiveFrame,
}
export const ROASTING_GUIDELINES = {
max_intensity: 0.8,
cooldown: 3,
intensity_by_trust: {
low: 0.3,
medium: 0.6,
high: 0.8,
},
avoid_topics: [
'appearance', 'disability', 'trauma', 'loss',
'identity', 'beliefs', 'family', 'health',
],
safe_targets: [
'coding_bugs', 'game_performance', 'silly_mistakes',
'predictable_behavior', 'tool_failures',
],
}
export const METACOGNITION_THRESHOLDS = {
low_confidence: 0.4,
low_reasoning_quality: 0.5,
high_cognitive_load: 0.8,
frame_lock_duration: 10,
}