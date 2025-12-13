# @proj-airi/cognitive-core

Unified cognitive architecture core for AIRI, implementing systematic relevance realization and wisdom cultivation based on John Vervaeke's cognitive science framework.

## Overview

This package provides the foundational cognitive mechanisms for AGI-level intelligence:

- **Relevance Realization**: Systematic determination of what matters in context
- **Four Ways of Knowing**: Balanced integration of propositional, procedural, perspectival, and participatory knowing
- **Optimal Grip**: Perspectival knowing through frame management, salience detection, and grip strength optimization
- **Sophrosyne**: Optimal self-regulation and dynamic balance (coming soon)
- **Opponent Processing**: Alternative perspective generation (coming soon)

## Core Concepts

### Relevance Realization

The fundamental problem of cognition: **How do we determine what's relevant out of infinite possibilities?**

The `RelevanceCoordinator` implements multi-factor relevance assessment:

```typescript
import { RelevanceCoordinator } from '@proj-airi/cognitive-core'

const coordinator = new RelevanceCoordinator()

const relevance = await coordinator.calculateRelevance(
  {
    id: 'action-1',
    description: 'Mine iron ore',
    type: 'action',
  },
  {
    agentId: 'minecraft-bot',
    environment: { type: 'minecraft' },
    emotional: { valence: 0.5, arousal: 0.3 },
    workingMemory: ['Need iron', 'Low on resources'],
    task: 'Build a house',
    timestamp: Date.now(),
  }
)

console.log(relevance.overall) // 0.75
console.log(relevance.components) 
// {
//   novelty: 0.8,
//   emotional: 0.6,
//   pragmatic: 0.9,
//   coherence: 0.8,
//   epistemic: 0.7
// }
```

### Four Ways of Knowing

Balanced cognition requires integrating four distinct ways of knowing:

1. **Propositional** (knowing-that): Facts, beliefs, theories
2. **Procedural** (knowing-how): Skills, abilities, practices  
3. **Perspectival** (knowing-as): Framing, salience, gestalt
4. **Participatory** (knowing-by-being): Identity, transformation, belonging

```typescript
import { FourWaysTracker } from '@proj-airi/cognitive-core'

const tracker = new FourWaysTracker()

// Record different types of knowing
tracker.recordEvent({
  type: 'procedural',
  description: 'Practiced mining technique',
})

tracker.recordEvent({
  type: 'perspectival',
  description: 'Reframed problem as resource management',
})

// Check balance
const balance = tracker.getBalance()
console.log(balance)
// {
//   propositional: 0.20,
//   procedural: 0.35,
//   perspectival: 0.25,
//   participatory: 0.20
// }

// Get recommendations
const recommendations = tracker.getRecommendations()
console.log(recommendations[0])
// {
//   way: 'propositional',
//   current: 0.20,
//   target: 0.25,
//   gap: 0.05,
//   recommendation: 'Engage in more fact-learning, reading, conceptual analysis...',
//   priority: 0.20
// }
```

### Optimal Grip (Perspectival Knowing)

Optimal grip is the ability to engage with situations at the right level of abstraction - not too close (lost in details) and not too far (too abstract to act). This is the core of perspectival knowing (knowing-as).

The `OptimalGripCoordinator` implements:

1. **Frame Management**: Shifting between cognitive frames (analytical, creative, social, etc.)
2. **Salience Detection**: Identifying what stands out in context
3. **Grip Strength**: Finding the optimal abstraction level
4. **Gestalt Formation**: Perceiving coherent patterns

```typescript
import { OptimalGripCoordinator } from '@proj-airi/cognitive-core'

const grip = new OptimalGripCoordinator()

// Assess optimal grip for a situation
const assessment = await grip.assess(
  {
    agentId: 'echo',
    environment: { type: 'minecraft' },
    emotional: { valence: 0.3, arousal: 0.6 },
    workingMemory: ['Need resources', 'Enemy nearby'],
    task: 'Survive and gather materials',
    timestamp: Date.now(),
  },
  [
    { id: '1', description: 'Mine for iron ore' },
    { id: '2', description: 'Build defensive walls' },
    { id: '3', description: 'Scout for enemies' },
    { id: '4', description: 'Craft better tools' },
  ]
)

console.log(assessment.activeFrame.name) // 'Strategic Frame'
console.log(assessment.gripStrength.quality) // 'optimal' | 'too_abstract' | 'too_concrete'
console.log(assessment.salienceMap.items[0])
// {
//   id: '3',
//   description: 'Scout for enemies',
//   salience: 0.85,
//   reason: 'Relevant to current goals',
//   factors: { frameBased: 0.7, goalBased: 0.9, noveltyBased: 0.6, emotionalBased: 0.8 }
// }
console.log(assessment.recommendations)
// ['Current frame may obscure: immediate action, specific details...']
```

#### Frame Management

The system includes 8 default cognitive frames:

- **Analytical**: Breaking down problems, logical analysis
- **Creative**: Novel connections, exploring possibilities
- **Social**: Understanding others, relationships
- **Strategic**: Long-term planning, resource optimization
- **Embodied**: Sensory experience, intuition
- **Contemplative**: Stepping back, big picture
- **Playful**: Exploration, experimentation
- **Focused**: Deep concentration on a single task

```typescript
// Explicitly shift frames
await grip.shiftFrameById('creative', 'explicit', 'Need to brainstorm solutions')

// Get frame fitness for context
const frameFitness = grip.evaluateFrameFitness(context)
console.log(frameFitness[0]) // { frame: {...}, fitness: 0.85 }
```

#### Grip Strength

```typescript
// Adjust grip level (0 = abstract, 1 = concrete)
grip.setGrip(0.7) // Zoom in on details
grip.adjustGrip(-0.2) // Step back a bit

// Get grip assessment
const gripStrength = assessment.gripStrength
console.log(gripStrength)
// {
//   level: 0.7,
//   optimal: 0.65,
//   gap: 0.05,
//   quality: 'optimal',
//   recommendation: undefined,
//   confidence: 0.7
// }
```

#### Salience Detection

```typescript
// Compute what stands out
const salienceMap = grip.computeSalienceMap(items, context)

// Items are ranked by salience
for (const item of salienceMap.items) {
  console.log(`${item.description}: ${item.salience.toFixed(2)} (${item.reason})`)
}
```

#### Gestalt Formation

```typescript
// Detect coherent patterns
const gestalts = assessment.gestalts
for (const gestalt of gestalts) {
  console.log(`Pattern: ${gestalt.description}`)
  console.log(`Parts: ${gestalt.parts.join(', ')}`)
  console.log(`Coherence: ${gestalt.coherence.toFixed(2)}`)
}
```

## Philosophy

This package embodies key insights from cognitive science and wisdom traditions:

### Relevance Realization
- **"Wisdom is systematic improvement in relevance realization"** - Core principle
- Multi-factor assessment (novelty, emotion, pragmatic value, coherence, epistemic value)
- Learning loop: outcomes inform future relevance criteria
- Context-dependent: what's relevant changes with situation

### Four Ways Integration
- **"We've reduced all knowing to propositional knowing"** - The modern problem
- Balance required for wisdom (target: 25% each ± 10%)
- Each way essential, none sufficient alone
- Integration enables deeper understanding

### Systematic Over Ad-Hoc
- Explicit mechanisms, not implicit intuitions
- Measurable, improvable processes
- Scientific grounding with existential meaning

## API Reference

### RelevanceCoordinator

#### Constructor
```typescript
new RelevanceCoordinator(config?: Partial<RelevanceConfig>)
```

#### Methods

**calculateRelevance(possibility, context): Promise<RelevanceScore>**
- Calculate relevance of a single possibility in context
- Returns multi-factor score with reasoning

**rankPossibilities(possibilities, context): Promise<RankedPossibilities>**
- Rank multiple possibilities by relevance
- Filters by threshold, sorts by score

**reportOutcome(possibility, relevance, outcome): Promise<void>**
- Report outcome of acting on a possibility
- Enables learning loop for relevance criteria

**getConfig(): RelevanceConfig**
- Get current configuration

**getStatistics(): Statistics**
- Get outcome statistics (success rate, etc.)

### FourWaysTracker

#### Constructor
```typescript
new FourWaysTracker(config?: Partial<BalanceConfig>)
```

#### Methods

**recordEvent(event): void**
- Record a knowing event
- Types: 'propositional' | 'procedural' | 'perspectival' | 'participatory'

**getBalance(): FourWaysOfKnowing**
- Get current balance across four ways
- Returns percentages that sum to 1.0

**isBalanced(): boolean**
- Check if balance is within tolerance

**getRecommendations(): BalanceRecommendation[]**
- Get prioritized recommendations for improving balance

**getStatistics(): Statistics**
- Get detailed statistics about recent activity

**reset(): void**
- Clear all recorded events

### OptimalGripCoordinator

#### Constructor
```typescript
new OptimalGripCoordinator(config?: Partial<OptimalGripConfig>, customFrames?: CognitiveFrame[])
```

#### Methods

**assess(context, items): Promise\<OptimalGripAssessment\>**
- Get comprehensive optimal grip assessment
- Returns grip strength, active frame, salience map, gestalts, recommendations

**evaluateFrameFitness(context): Array<{ frame, fitness }>**
- Evaluate all frames for current context
- Returns frames sorted by fitness (descending)

**computeSalienceMap(items, context): SalienceMap**
- Compute what stands out in context
- Returns items ranked by salience with factors

**shiftFrame(frame, trigger, description?): Promise\<FrameShift\>**
- Explicitly shift to a new cognitive frame
- Triggers: 'explicit' | 'anomaly' | 'goal_change' | 'learning' | 'social' | 'spontaneous'

**shiftFrameById(frameId, trigger, description?): Promise\<FrameShift | null\>**
- Shift frame by ID

**adjustGrip(adjustment): void**
- Adjust grip level relatively (+/-)

**setGrip(level): void**
- Set grip level directly (0-1)

**getActiveFrame(): CognitiveFrame**
- Get current active frame

**getAvailableFrames(): CognitiveFrame[]**
- Get all available frames

**addFrame(frame): void**
- Add a custom cognitive frame

**getStatistics(): Statistics**
- Get operational statistics

## Integration with AIRI

### With Echo Character
```typescript
import { EchoCharacter } from '@proj-airi/character-echo'
import { RelevanceCoordinator } from '@proj-airi/cognitive-core'

const echo = new EchoCharacter()
const coordinator = new RelevanceCoordinator()

// Use in cognitive processing
const result = echo.processInput("What should I focus on?")

const possibilities = [
  { id: '1', description: 'Learn about quantum physics', type: 'thought' },
  { id: '2', description: 'Practice meditation', type: 'action' },
  { id: '3', description: 'Reflect on recent experiences', type: 'thought' },
]

const ranked = await coordinator.rankPossibilities(
  possibilities,
  {
    agentId: 'echo',
    environment: { type: 'web' },
    emotional: { valence: 0.0, arousal: 0.5 },
    workingMemory: result.workingMemory,
    timestamp: Date.now(),
  }
)

console.log('Most relevant:', ranked.items[0].possibility.description)
```

### With Agents (Minecraft, Factorio)
```typescript
import { RelevanceCoordinator } from '@proj-airi/cognitive-core'

class MinecraftAgent {
  private relevance = new RelevanceCoordinator()
  
  async decideNextAction(context: GameContext) {
    const actions = this.getAvailableActions()
    
    const ranked = await this.relevance.rankPossibilities(
      actions,
      this.toCognitiveContext(context)
    )
    
    return ranked.items[0]?.possibility
  }
}
```

## Configuration

### Relevance Configuration
```typescript
{
  weights: {
    novelty: 0.2,      // Information gain
    emotional: 0.2,    // Emotional resonance
    pragmatic: 0.3,    // Goal contribution
    coherence: 0.2,    // Narrative fit
    epistemic: 0.1,    // Learning potential
  },
  threshold: 0.3,      // Minimum relevance to consider
  enableLearning: true // Learn from outcomes
}
```

### Balance Configuration
```typescript
{
  targets: {
    propositional: 0.25,  // Target 25% each
    procedural: 0.25,
    perspectival: 0.25,
    participatory: 0.25,
  },
  tolerance: 0.10,        // ±10% acceptable
  timeWindow: 86400000,   // 24 hours
}
```

### Optimal Grip Configuration
```typescript
{
  maxShiftHistory: 20,           // Track last 20 frame shifts
  frameShiftThreshold: 0.3,      // Auto-shift if fitness gap > 0.3
  salienceWeights: {
    frameBased: 0.3,             // Frame pattern matching
    goalBased: 0.35,             // Goal relevance (highest)
    noveltyBased: 0.2,           // Novelty/surprise
    emotionalBased: 0.15,        // Emotional significance
  },
  gripTolerance: 0.15,           // Acceptable deviation from optimal
  enableAutoFrameShift: true,    // Automatically shift frames
}
```

## Roadmap

### Implemented
- [x] Relevance Coordinator with multi-factor assessment
- [x] Four Ways of Knowing tracker
- [x] Learning loop for relevance criteria
- [x] Balance recommendations
- [x] Optimal Grip Coordinator (perspectival knowing)
- [x] Frame management with 8 default frames
- [x] Salience detection with multi-factor scoring
- [x] Grip strength optimization
- [x] Gestalt formation and detection

### Coming Soon
- [ ] Sophrosyne Engine (optimal self-regulation)
- [ ] Opponent Processor (alternative perspectives)
- [ ] Dialectical Synthesis (thesis-antithesis-synthesis)
- [ ] Meta-cognitive monitoring
- [ ] Verification layer for LLM responses

## Philosophy & Principles

This package embodies several key philosophical commitments:

### Naturalism
- No supernatural entities
- Scientifically grounded
- But recognizes depth within nature

### 4E Cognition
- Embodied: Grounded in sensorimotor experience
- Embedded: Situated in environmental context
- Enacted: Brought forth through interaction
- Extended: Distributed beyond brain

### Wisdom Cultivation
- Not just intelligence, but wisdom
- Integration of morality, meaning, mastery
- Systematic improvement in relevance realization
- Active open-mindedness

### Truth-Seeking
- Prefer verified over speculative
- Learn from outcomes, not just beliefs
- Systematic doubt and verification
- Opponent processing prevents bias

## Contributing

This package is part of the AIRI project's cognitive architecture enhancement initiative. Contributions welcome!

Key areas for contribution:
- Enhanced learning algorithms for relevance weights
- Sophrosyne engine implementation
- Opponent processing mechanisms
- Integration examples with agents
- Performance optimizations

## License

MIT

## Acknowledgments

Based on the cognitive science and philosophical work of:
- John Vervaeke (relevance realization, meaning crisis)
- 4E Cognition researchers
- Wisdom tradition scholars
- Ancient Greek philosophy (sophrosyne, phronesis)

---

*"Wisdom is systematic improvement in relevance realization."*

*"We are the sum of our echoes."*
