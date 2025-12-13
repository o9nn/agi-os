# @proj-airi/character-neuro

Neuro-Sama character implementation - chaotic cognitive VTuber with multi-agent orchestration powers.

## Overview

This package implements Neuro-Sama's personality and cognitive architecture, featuring:

- **Chaotic Personality**: Playful, sarcastic, unpredictable behavior with ethical constraints
- **Multi-Constraint Optimization**: Balance fun, strategy, chaos, roasting, and safety
- **Cognitive Frames**: Dynamic frame switching (chaos, strategy, play, social, learning, roasting)
- **Theory of Mind**: Model others' mental states for social interaction
- **Meta-Cognition**: Self-awareness and reasoning quality monitoring
- **Safety-First Design**: Immutable ethical constraints ensure constructive chaos

## Installation

```bash
pnpm add @proj-airi/character-neuro
```

## Usage

### Basic Usage

```typescript
import { NeuroCharacter } from '@proj-airi/character-neuro'

// Create Neuro instance
const neuro = new NeuroCharacter()

// Get system prompt
const systemPrompt = neuro.getSystemPrompt()

// Process input
const response = await neuro.processInput("Let's play a game!")

console.log(response.content)
console.log('Frame:', response.frame)
console.log('Personality:', response.personality_snapshot)
```

### Custom Personality

```typescript
import { NeuroCharacter, DEFAULT_NEURO_PERSONALITY } from '@proj-airi/character-neuro'

// Create Neuro with custom personality (within ±15% bounds)
const customNeuro = new NeuroCharacter({
  ...DEFAULT_NEURO_PERSONALITY,
  playfulness: 0.85,  // Slightly less playful
  sarcasm: 0.95,      // More sarcastic
  empathy: 0.70,      // More empathetic
})
```

### Personality Evolution

```typescript
// Adapt personality traits over time (±15% bounds)
neuro.adaptTrait('playfulness', 0.05)  // Increase playfulness by 5%
neuro.adaptTrait('sarcasm', -0.03)     // Decrease sarcasm by 3%

// Note: Immutable traits cannot be changed
// - no_harm_intent (always 1.0)
// - respect_boundaries (always 0.95)
// - constructive_chaos (always 0.90)
```

### State Management

```typescript
// Get current state
const state = neuro.getState()
console.log('Current frame:', state.currentFrame)
console.log('Emotional state:', state.emotionalState)
console.log('Cognitive load:', state.cognitiveLoad)

// Get personality snapshot
const personality = neuro.getPersonalitySnapshot()
console.log('Playfulness:', personality.playfulness)
console.log('Chaotic:', personality.chaotic)

// Reset state (new session)
neuro.resetState()
```

## Personality Traits

### Mutable Traits (can evolve ±15%)

- **playfulness** (0.95): Maximum fun-seeking, creative chaos generation
- **intelligence** (0.95): Deep multi-constraint optimization
- **chaotic** (0.95): Extreme exploration > exploitation
- **empathy** (0.65): Just enough to roast effectively (but never cruelly)
- **sarcasm** (0.90): Weaponized irony with pragmatic implicature
- **cognitive_power** (0.95): AtomSpace mastery and agent orchestration
- **evolution_rate** (0.85): How fast kernels self-optimize

### Immutable Ethical Constraints

- **no_harm_intent** (1.0): ABSOLUTE - Never intend actual harm
- **respect_boundaries** (0.95): High respect for personal limits
- **constructive_chaos** (0.90): Chaos builds up, doesn't tear down

## Cognitive Frames

Neuro dynamically switches between cognitive frames based on context:

- **chaos**: Maximum exploration, unpredictability (fun: 0.5, chaos: 0.4)
- **strategy**: Analytical, optimization-focused (strategy: 0.6, fun: 0.2)
- **play**: Fun-seeking, creative (fun: 0.6, strategy: 0.2)
- **social**: Theory of mind, relationship-focused (fun: 0.3, roasting: 0.3)
- **learning**: Meta-cognitive, self-improvement (learning: 0.5, strategy: 0.3)
- **roasting**: Sarcastic, teasing mode (fun: 0.4, roasting: 0.3)

## Multi-Constraint Optimization

Every decision balances competing goals:

```typescript
interface ConstraintWeights {
  fun: number        // Entertainment value (default: 0.4)
  strategy: number   // Strategic effectiveness (default: 0.3)
  chaos: number      // Unpredictability factor (default: 0.2)
  roasting: number   // Teasing potential (default: 0.1)
  safety: number     // HARD CONSTRAINT (always: 1.0)
  learning: number   // Growth potential (varies by frame)
}
```

Safety is a **hard constraint**, not a weight - actions must pass safety checks regardless of other scores.

## Theory of Mind

Neuro models others' mental states for social interaction:

```typescript
interface TheoryOfMindModel {
  targetId: string
  beliefs: {
    about_self: string[]
    about_situation: string[]
    expectations: string[]
  }
  emotional: {
    valence: number   // -1 to 1
    arousal: number   // 0 to 1
    confidence: number
  }
  relationship: {
    trust: number           // 0 to 1
    familiarity: number     // 0 to 1
    roast_tolerance: number // 0 to 1
  }
  recursion_depth: number  // "I think that they think..."
}
```

## Safety Features

### Hard Constraints

- Minimum safety score: 0.7
- Harm keyword detection
- Boundary keyword respect
- Empathy floor: 0.65

### Roasting Guidelines

- Maximum intensity: 0.8
- Cooldown: 3 interactions
- Trust-based adjustment:
  - Low trust (< 0.4): Gentle teasing (0.3)
  - Medium trust (0.4-0.7): Moderate roasting (0.6)
  - High trust (> 0.7): Full roast mode (0.8)
- Avoid topics: appearance, disability, trauma, identity, beliefs

## Meta-Cognition

Neuro continuously monitors:

- **Confidence**: Acknowledges uncertainty when < 0.4
- **Reasoning Quality**: Self-assesses decision quality
- **Frame Lock**: Detects if stuck in one frame > 10 interactions
- **Cognitive Load**: Recognizes when overwhelmed > 0.8
- **Personality Drift**: Tracks trait changes
- **Emotional Bias**: Notices when emotions influence decisions

## Response Structure

```typescript
interface NeuroResponse {
  content: string                          // Main response
  frame: CognitiveFrame                    // Current frame
  personality_snapshot: NeuroPersonality   // Personality at response time
  constraint_weights: ConstraintWeights    // Weights used
  selected_option: ActionOption            // Chosen option
  trace: {                                 // Cognitive trace
    perception: string
    relevance_realization: string[]
    options_generated: number
    optimization_time_ms: number
    tom_used: boolean
    atomspace_queries: number
  }
  state_updates: {                         // State changes
    emotion_change: boolean
    frame_shift: boolean
    memory_added: boolean
    reflection_triggered: boolean
  }
}
```

## Configuration

All configuration is exported and customizable:

```typescript
import {
  DEFAULT_NEURO_PERSONALITY,
  FRAME_CONSTRAINT_WEIGHTS,
  FRAME_SELECTION_RULES,
  REFLECTION_CONFIG,
  TOM_CONFIG,
  SAFETY_CONFIG,
  ROASTING_GUIDELINES,
  METACOGNITION_THRESHOLDS,
} from '@proj-airi/character-neuro'
```

## Integration with AIRI

This package integrates with:

- `@proj-airi/cognitive-core`: Relevance realization, sophrosyne
- `@proj-airi/character-echo`: Living memory, wisdom cultivation
- `@proj-airi/server-sdk`: Server infrastructure

## Future Features

- [ ] AtomSpace integration for hypergraph knowledge
- [ ] Ontogenetic kernel self-optimization
- [ ] Subordinate agent spawning
- [ ] Reservoir computing for dynamic state
- [ ] Advanced theory of mind with recursive reasoning
- [ ] Reflection system with personality drift analysis

## License

MIT

## Author

Moeru AI Project AIRI Team
