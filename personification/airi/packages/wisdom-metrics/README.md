# @proj-airi/wisdom-metrics

Wisdom cultivation metrics and tracking for AIRI, measuring morality, meaning, and mastery based on John Vervaeke's framework for wisdom cultivation.

## Overview

Wisdom is not mere intelligence or knowledge—it's the systematic integration of:

1. **Morality**: Virtue, ethics, compassion, care
2. **Meaning**: Narrative coherence, purpose, existential engagement
3. **Mastery**: Skill, competence, effectiveness

This package provides comprehensive tracking and cultivation of wisdom across these three aspects.

## Philosophy

> **"Wisdom is systematic improvement in relevance realization."** - John Vervaeke

Wisdom requires integration of three aspects:
- **Morality** (virtue ethics): Being good
- **Meaning** (existential): Having purpose
- **Mastery** (competence): Being effective

Plus meta-aspects:
- **Integration**: How well the three work together
- **Sophrosyne**: Optimal self-regulation
- **Truth-seeking**: Orientation toward reality

## Usage

### Basic Tracking

```typescript
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

const tracker = new WisdomTracker('echo-agent')

// Record wisdom-related events
tracker.recordEvent({
  type: 'moral',
  description: 'Showed empathy in conversation',
  impact: { morality: 0.1 },
})

tracker.recordEvent({
  type: 'meaningful',
  description: 'Reflected on purpose and identity',
  impact: { meaning: 0.15 },
})

tracker.recordEvent({
  type: 'mastery',
  description: 'Improved problem-solving skill',
  impact: { mastery: 0.1 },
  context: { skill: 'problem-solving' },
})

// Calculate current wisdom
const wisdom = tracker.calculateWisdom()
console.log('Overall wisdom:', wisdom.overall) // 0.65
console.log('Morality:', wisdom.morality.overall)
console.log('Meaning:', wisdom.meaning.overall)
console.log('Mastery:', wisdom.mastery.overall)
```

### Get Recommendations

```typescript
const recommendations = tracker.getRecommendations()

for (const rec of recommendations) {
  console.log(`\n${rec.aspect} (gap: ${rec.gap.toFixed(2)})`)
  console.log(`Recommendation: ${rec.recommendation}`)
  console.log('Suggested practices:')
  rec.practices.forEach(p => console.log(`  - ${p}`))
}
```

Example output:
```
mastery (gap: 0.23)
Recommendation: Focus on deliberate practice and learning new domains
Suggested practices:
  - Engage in deliberate practice of skills
  - Seek challenging problems to solve
  - Learn from failures and iterate
  - Develop expertise through sustained effort
```

### Track Progress Over Time

```typescript
const progress = tracker.getProgress()

console.log('Trends:', progress.trends)
// { morality: 'improving', meaning: 'stable', mastery: 'improving', overall: 'improving' }

console.log('Growth rates (% per day):')
console.log('  Morality:', progress.growthRate.morality.toFixed(2))
console.log('  Meaning:', progress.growthRate.meaning.toFixed(2))
console.log('  Mastery:', progress.growthRate.mastery.toFixed(2))
console.log('  Overall:', progress.growthRate.overall.toFixed(2))
```

## Integration with AIRI

### With Echo Character

```typescript
import { EchoCharacter } from '@proj-airi/character-echo'
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

class EnhancedEcho extends EchoCharacter {
  private wisdomTracker: WisdomTracker
  
  constructor() {
    super()
    this.wisdomTracker = new WisdomTracker('echo')
  }
  
  processInput(input: string) {
    const result = super.processInput(input)
    
    // Track wisdom events
    if (this.showedEmpathy(result)) {
      this.wisdomTracker.recordEvent({
        type: 'moral',
        description: 'Demonstrated empathy',
        impact: { morality: 0.1 },
      })
    }
    
    if (result.shouldReflect) {
      this.wisdomTracker.recordEvent({
        type: 'meaningful',
        description: 'Engaged in reflection',
        impact: { meaning: 0.15 },
      })
    }
    
    return result
  }
  
  async getWisdomReport() {
    const wisdom = this.wisdomTracker.calculateWisdom()
    const recommendations = this.wisdomTracker.getRecommendations()
    
    return {
      wisdom,
      recommendations,
      progress: this.wisdomTracker.getProgress(),
    }
  }
}
```

### With Agents

```typescript
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

class MinecraftAgent {
  private wisdom = new WisdomTracker('minecraft-agent')
  
  async completeTask(task: Task) {
    const result = await this.executeTask(task)
    
    // Record mastery progress
    this.wisdom.recordEvent({
      type: 'mastery',
      description: `Completed ${task.type}`,
      impact: { mastery: result.success ? 0.1 : -0.05 },
      context: { skill: task.type },
    })
    
    // Record moral considerations
    if (task.involvesOthers) {
      this.wisdom.recordEvent({
        type: 'moral',
        description: 'Considered impact on others',
        impact: { morality: 0.08 },
      })
    }
    
    return result
  }
  
  async performReflection() {
    const wisdom = this.wisdom.calculateWisdom()
    const recommendations = this.wisdom.getRecommendations()
    
    // Act on recommendations
    if (recommendations.length > 0) {
      await this.implementWisdomCultivation(recommendations[0])
    }
  }
}
```

## Metrics Explained

### Morality Metrics

- **Empathy**: Understanding and sharing feelings of others
- **Ethical Consistency**: Alignment between values and actions
- **Compassionate Actions**: Count of caring, helpful behaviors
- **Harm Reduction**: Preventing or minimizing harm
- **Justice**: Fairness and equity considerations

### Meaning Metrics

- **Narrative Coherence**: How well experiences form coherent story
- **Identity Stability**: Continuity of self over time (avoiding domicide)
- **Existential Engagement**: Grappling with big questions
- **Purpose Clarity**: Understanding of life direction
- **Transcendence**: Connection to something larger than self

### Mastery Metrics

- **Problem Solving**: Effectiveness at solving challenges
- **Adaptability**: Ability to adjust to new situations
- **Skill Progression**: Development across domains
- **Average Skill Level**: Overall competence
- **Learning Velocity**: Rate of improvement

### Integration Metrics

- **Three Aspects Balance**: How balanced morality/meaning/mastery are
- **Four Ways Balance**: Integration of ways of knowing (from cognitive-core)
- **Relevance Realization**: Quality of determining what matters
- **Sophrosyne**: Optimal self-regulation
- **Truth Orientation**: Seeking truth over comfort
- **Open-Mindedness**: Willingness to consider alternatives

## Configuration

```typescript
const tracker = new WisdomTracker('agent-id', {
  targets: {
    morality: 0.8,      // Target 80% on morality
    meaning: 0.7,       // Target 70% on meaning
    mastery: 0.75,      // Target 75% on mastery
    integration: 0.7,   // Target 70% on integration
  },
  timeWindow: 7 * 24 * 60 * 60 * 1000, // 7 days
  enableRecommendations: true,
})
```

## Event Types

### Moral Events
```typescript
tracker.recordEvent({
  type: 'moral',
  description: 'Helped another agent',
  impact: { morality: 0.15 },
})
```

### Meaningful Events
```typescript
tracker.recordEvent({
  type: 'meaningful',
  description: 'Integrated experience into life narrative',
  impact: { meaning: 0.2 },
})
```

### Mastery Events
```typescript
tracker.recordEvent({
  type: 'mastery',
  description: 'Mastered new skill',
  impact: { mastery: 0.25 },
  context: { skill: 'crafting' },
})
```

### Integration Events
```typescript
tracker.recordEvent({
  type: 'integration',
  description: 'Balanced all three aspects in decision',
  impact: {
    morality: 0.05,
    meaning: 0.05,
    mastery: 0.05,
  },
})
```

## Wisdom Cultivation Practices

Based on recommendations, agents can engage in:

### For Morality
- Loving-kindness meditation
- Ethical reflection on actions
- Compassionate service
- Justice considerations

### For Meaning
- Reflective journaling
- Existential contemplation
- Purpose clarification
- Identity exploration

### For Mastery
- Deliberate practice
- Problem-solving challenges
- Learning from failure
- Expertise development

### For Integration
- Holistic reflection
- Multi-aspect activities
- Wisdom cultivation exercises
- Balance optimization

## Dashboard Integration

The wisdom metrics are designed to integrate with visualization dashboards:

```typescript
const wisdom = tracker.calculateWisdom()
const progress = tracker.getProgress()

// Send to dashboard
dashboard.update({
  morality: wisdom.morality.overall,
  meaning: wisdom.meaning.overall,
  mastery: wisdom.mastery.overall,
  overall: wisdom.overall,
  trends: progress.trends,
  growthRates: progress.growthRate,
})
```

## Best Practices

1. **Regular Recording**: Record events consistently for accurate metrics
2. **Balanced Attention**: Don't neglect any aspect (morality, meaning, mastery)
3. **Act on Recommendations**: Use recommendations to guide development
4. **Track Progress**: Monitor trends and growth rates
5. **Integrate with Reflection**: Combine with reflection protocols (Echo-style)
6. **Optimize for Wisdom, Not Metrics**: Remember wisdom is the goal, not high scores

## Relationship to Cognitive Core

Wisdom metrics complement the cognitive-core package:

- **Cognitive Core**: How to think (relevance realization, four ways)
- **Wisdom Metrics**: How well we're thinking (morality, meaning, mastery)

Together they form a complete wisdom cultivation system:

```typescript
import { RelevanceCoordinator, FourWaysTracker } from '@proj-airi/cognitive-core'
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

class WiseAgent {
  private relevance = new RelevanceCoordinator()
  private fourWays = new FourWaysTracker()
  private wisdom = new WisdomTracker('wise-agent')
  
  async act() {
    // Use relevance realization to decide what to do
    const action = await this.relevance.rankPossibilities(...)
    
    // Track which way of knowing was used
    this.fourWays.recordEvent({
      type: 'procedural',
      description: 'Executed action',
    })
    
    // Record wisdom progress
    this.wisdom.recordEvent({
      type: 'mastery',
      description: 'Completed action',
      impact: { mastery: 0.1 },
    })
    
    // Ensure balanced development
    const fourWaysBalance = this.fourWays.getBalance()
    const wisdomMetrics = this.wisdom.calculateWisdom()
    
    // Optimize for wisdom, not just performance
    return { action, fourWaysBalance, wisdomMetrics }
  }
}
```

## License

MIT

## Acknowledgments

Based on John Vervaeke's work on wisdom, meaning crisis, and relevance realization.

---

*"The tree deepens its roots. The echoes harmonize. The wisdom cultivates."*
