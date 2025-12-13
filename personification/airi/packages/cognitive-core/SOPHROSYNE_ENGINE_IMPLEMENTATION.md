# Sophrosyne Engine Implementation Guide

**Component:** Optimal Self-Regulation System  
**Package:** `@proj-airi/cognitive-core`  
**Priority:** HIGHEST  
**Philosophy:** Ancient Greek virtue of optimal balance - neither excess nor deficiency

---

## Overview

The Sophrosyne Engine implements **dynamic optimal self-regulation** - the ability to find context-dependent balance between competing demands. Unlike static moderation, sophrosyne is the art of finding the right balance for each unique situation.

### Key Principle

> **"The mean is not a mathematical midpoint, but the contextually optimal point."**

- Low stakes → Can explore more (risk acceptable)
- High stakes → Should exploit known strategies (safety critical)
- Novel situation → Need more information (breadth before depth)
- Familiar situation → Can execute efficiently (depth over breadth)

---

## Core Tradeoffs to Balance

### 1. Exploration vs Exploitation

**Spectrum:**
- Exploration (0.0): Try new things, discover possibilities
- Exploitation (1.0): Use known best strategies

**Context Factors:**
- Stakes: High stakes → exploit more
- Uncertainty: High uncertainty → explore more
- Time pressure: High pressure → exploit (no time to experiment)
- Current performance: Poor performance → explore (need better strategies)

### 2. Speed vs Accuracy

**Spectrum:**
- Speed (0.0): Fast decisions, accept errors
- Accuracy (1.0): Slow careful decisions, minimize errors

**Context Factors:**
- Time available: Limited time → speed
- Error cost: High cost → accuracy
- Task complexity: Complex → accuracy
- Stakes: High stakes → accuracy

### 3. Breadth vs Depth

**Spectrum:**
- Breadth (0.0): Surface level, many topics
- Depth (1.0): Deep dive, single focus

**Context Factors:**
- Understanding level: Low understanding → breadth first
- Goal specificity: Specific goal → depth
- Time available: Limited → prioritize depth in key areas
- Novelty: Novel domain → breadth to map terrain

### 4. Interruption vs Persistence

**Spectrum:**
- Interruption (0.0): Quick context switching
- Persistence (1.0): Deep sustained focus

**Context Factors:**
- Task progress: Near completion → persist
- Better opportunity: Compelling alternative → interrupt
- Diminishing returns: Progress slowing → interrupt
- Flow state: In flow → persist

### 5. Risk vs Safety

**Spectrum:**
- Risk-taking (0.0): Bold, experimental
- Safety (1.0): Conservative, cautious

**Context Factors:**
- Stakes: High stakes → safety
- Resources: Abundant resources → can risk more
- Learning opportunity: High learning value → acceptable risk
- Failure cost: High cost → safety

---

## Architecture

```typescript
// packages/cognitive-core/src/sophrosyne-engine.ts

export interface Spectrum {
  name: string
  // NOTE: 0.0 and 1.0 are normalized bounds for all spectra (design choice for consistency)
  // All spectrum calculations work in normalized 0-1 space, then interpret contextually
  min: { name: string; value: 0.0 }
  max: { name: string; value: 1.0 }
  currentPosition: number  // 0.0 to 1.0
}

export interface Context {
  stakes: number            // 0 (trivial) to 1 (critical)
  uncertainty: number       // 0 (certain) to 1 (unknown)
  timeAvailable: number     // 0 (urgent) to 1 (ample)
  resources: number         // 0 (scarce) to 1 (abundant)
  taskProgress: number      // 0 (just started) to 1 (nearly complete)
  currentPerformance: number // 0 (poor) to 1 (excellent)
  complexity: number        // 0 (simple) to 1 (complex)
  novelty: number          // 0 (familiar) to 1 (novel)
  errorCost: number        // 0 (low cost) to 1 (high cost)
  learningValue: number    // 0 (low) to 1 (high)
}

export interface OptimalPoint {
  position: number         // The optimal position (0.0 to 1.0)
  confidence: number       // How confident are we (0.0 to 1.0)
  reasoning: string        // Why this is optimal
  factors: ContextFactor[] // Which factors influenced this
}

export interface RegulationDecision {
  action: 'continue' | 'adjust' | 'switch'
  confidence: number
  reasoning: string
  adjustment?: number      // If action is 'adjust', how much (-1.0 to 1.0)
  switchTo?: Strategy      // If action is 'switch', switch to what
}

export class SophrosyneEngine {
  private regulationHistory: RegulationHistory
  private contextEvaluator: ContextEvaluator
  private optimalCalculator: OptimalCalculator
  
  /**
   * Calculate optimal position on a spectrum given context
   */
  async calculateOptimal(
    spectrum: Spectrum,
    context: Context
  ): Promise<OptimalPoint> {
    // Get historical data for similar contexts
    const historical = await this.regulationHistory.getSimilarContexts(
      spectrum.name,
      context,
      { limit: 20, similarityThreshold: 0.7 }
    )
    
    // Calculate base optimal position from context factors
    const baseOptimal = this.calculateBaseOptimal(spectrum, context)
    
    // Adjust based on historical outcomes
    const historicalAdjustment = this.getHistoricalAdjustment(
      historical,
      baseOptimal
    )
    
    // Calculate confidence based on agreement
    const confidence = this.calculateConfidence(
      baseOptimal,
      historicalAdjustment,
      historical.length
    )
    
    const optimal = baseOptimal + historicalAdjustment
    
    return {
      position: this.clamp(optimal, 0, 1),
      confidence,
      reasoning: this.explainOptimal(spectrum, context, optimal),
      factors: this.getInfluentialFactors(spectrum, context)
    }
  }
  
  /**
   * Decide whether to continue, adjust, or switch strategy
   */
  async regulate(
    spectrum: Spectrum,
    context: Context
  ): Promise<RegulationDecision> {
    const optimal = await this.calculateOptimal(spectrum, context)
    const current = spectrum.currentPosition
    const gap = optimal.position - current
    
    // At optimal? Continue
    if (Math.abs(gap) < 0.1 && optimal.confidence > 0.7) {
      return {
        action: 'continue',
        confidence: optimal.confidence,
        reasoning: `Currently at optimal position (${current.toFixed(2)})`
      }
    }
    
    // Small adjustment needed?
    if (Math.abs(gap) < 0.3) {
      return {
        action: 'adjust',
        adjustment: gap,
        confidence: optimal.confidence,
        reasoning: `Adjust by ${gap.toFixed(2)} to reach optimal (${optimal.position.toFixed(2)})`
      }
    }
    
    // Large gap? Switch strategy
    return {
      action: 'switch',
      switchTo: this.getStrategyForPosition(spectrum, optimal.position),
      confidence: optimal.confidence,
      reasoning: `Current position (${current.toFixed(2)}) far from optimal (${optimal.position.toFixed(2)})`
    }
  }
  
  /**
   * Record outcome of a regulation decision for learning
   */
  async recordOutcome(
    spectrum: Spectrum,
    context: Context,
    decision: RegulationDecision,
    outcome: {
      success: boolean
      performance: number  // 0 to 1
      notes?: string
    }
  ): Promise<void> {
    await this.regulationHistory.store({
      spectrum: spectrum.name,
      context,
      decision,
      outcome,
      timestamp: Date.now()
    })
  }
  
  private calculateBaseOptimal(spectrum: Spectrum, context: Context): number {
    // Different spectra have different context mappings
    switch (spectrum.name) {
      case 'exploration-exploitation':
        return this.calculateExplorationExploitation(context)
      case 'speed-accuracy':
        return this.calculateSpeedAccuracy(context)
      case 'breadth-depth':
        return this.calculateBreadthDepth(context)
      case 'interruption-persistence':
        return this.calculateInterruptionPersistence(context)
      case 'risk-safety':
        return this.calculateRiskSafety(context)
      default:
        throw new Error(`Unknown spectrum: ${spectrum.name}`)
    }
  }
  
  private calculateExplorationExploitation(context: Context): number {
    // High stakes → exploit more (move toward 1.0)
    // High uncertainty → explore more (move toward 0.0)
    // Poor performance → explore more (need better strategies)
    // Time pressure → exploit (no time to experiment)
    
    let position = 0.5 // Start at middle
    
    position += context.stakes * 0.3           // High stakes → exploit
    position -= context.uncertainty * 0.3      // High uncertainty → explore
    position -= (1 - context.currentPerformance) * 0.2  // Poor performance → explore
    position += (1 - context.timeAvailable) * 0.2       // Time pressure → exploit
    
    return this.clamp(position, 0, 1)
  }
  
  private calculateSpeedAccuracy(context: Context): number {
    // Time pressure → speed (toward 0.0)
    // High error cost → accuracy (toward 1.0)
    // High stakes → accuracy
    // High complexity → accuracy
    
    let position = 0.5
    
    position -= (1 - context.timeAvailable) * 0.3  // Time pressure → speed
    position += context.errorCost * 0.3            // Error cost → accuracy
    position += context.stakes * 0.2               // High stakes → accuracy
    position += context.complexity * 0.2           // Complexity → accuracy
    
    return this.clamp(position, 0, 1)
  }
  
  private calculateBreadthDepth(context: Context): number {
    // Novelty → breadth first (toward 0.0)
    // Good understanding → can go deep (toward 1.0)
    // Specific goal → depth
    // Limited time → depth in key areas
    
    let position = 0.5
    
    position -= context.novelty * 0.3              // Novel → breadth
    position += context.currentPerformance * 0.2   // Good performance → depth
    position -= context.uncertainty * 0.2          // Uncertainty → breadth
    position += (1 - context.timeAvailable) * 0.3  // Limited time → focus depth
    
    return this.clamp(position, 0, 1)
  }
  
  private calculateInterruptionPersistence(context: Context): number {
    // Near completion → persist (toward 1.0)
    // Poor progress → consider interrupting (toward 0.0)
    // Diminishing returns → interrupt
    
    let position = 0.5
    
    position += context.taskProgress * 0.4     // Near done → persist
    position += context.currentPerformance * 0.3  // Going well → persist
    position -= context.novelty * 0.2          // Novel opportunity → might interrupt
    position += (1 - context.timeAvailable) * 0.1  // Time pressure → persist to finish
    
    return this.clamp(position, 0, 1)
  }
  
  private calculateRiskSafety(context: Context): number {
    // High stakes → safety (toward 1.0)
    // Abundant resources → can risk (toward 0.0)
    // High learning value → acceptable risk
    // High error cost → safety
    
    let position = 0.5
    
    position += context.stakes * 0.3           // High stakes → safety
    position += context.errorCost * 0.3        // Error cost → safety
    position -= context.resources * 0.2        // Resources → can risk
    position -= context.learningValue * 0.2    // Learning → acceptable risk
    
    return this.clamp(position, 0, 1)
  }
  
  // NOTE: Clamp is duplicated here for documentation completeness
  // In actual implementation, extract to shared utils (e.g., @proj-airi/cognitive-core/utils)
  private clamp(value: number, min: number, max: number): number {
    return Math.max(min, Math.min(max, value))
  }
}
```

---

## Integration Examples

### Example 1: Minecraft Agent Decision Making

```typescript
import { SophrosyneEngine } from '@proj-airi/cognitive-core'

class MinecraftAgent {
  private sophrosyne = new SophrosyneEngine()
  
  async decideExplorationStrategy() {
    const context = {
      stakes: 0.3,            // Moderate - not hardcore mode
      uncertainty: 0.7,       // High - new world, unexplored
      timeAvailable: 0.8,     // Plenty of time
      resources: 0.2,         // Low resources
      taskProgress: 0.1,      // Just started
      currentPerformance: 0.5, // Moderate success so far
      complexity: 0.6,        // Minecraft is moderately complex
      novelty: 0.8,          // New world is novel
      errorCost: 0.4,        // Death has moderate cost
      learningValue: 0.9     // High learning value in exploration
    }
    
    const spectrum = {
      name: 'exploration-exploitation',
      min: { name: 'Exploration', value: 0.0 },
      max: { name: 'Exploitation', value: 1.0 },
      currentPosition: 0.3  // Currently exploring
    }
    
    const decision = await this.sophrosyne.regulate(spectrum, context)
    
    if (decision.action === 'continue') {
      console.log('Continue exploring - optimal strategy')
    } else if (decision.action === 'adjust') {
      console.log(`Adjust exploration level by ${decision.adjustment}`)
    } else {
      console.log(`Switch strategy: ${decision.switchTo}`)
    }
  }
}
```

### Example 2: Learning System

```typescript
class LearningSystem {
  private sophrosyne = new SophrosyneEngine()
  
  async decideLearningDepth(topic: string, understanding: number) {
    const context = {
      stakes: 0.6,            // Moderate stakes
      uncertainty: 1 - understanding,  // Low understanding = high uncertainty
      timeAvailable: 0.5,     // Moderate time
      resources: 0.7,         // Good resources
      taskProgress: understanding,  // Progress = understanding
      currentPerformance: understanding,
      complexity: 0.7,        // Complex topic
      novelty: 1 - understanding,  // Novel if don't understand
      errorCost: 0.3,        // Low error cost in learning
      learningValue: 0.9     // High learning value
    }
    
    const spectrum = {
      name: 'breadth-depth',
      min: { name: 'Breadth', value: 0.0 },
      max: { name: 'Depth', value: 1.0 },
      currentPosition: understanding  // More understanding → deeper
    }
    
    const optimal = await this.sophrosyne.calculateOptimal(spectrum, context)
    
    if (optimal.position < 0.3) {
      return 'survey_topic'  // Breadth first
    } else if (optimal.position < 0.7) {
      return 'focused_study'  // Moderate depth
    } else {
      return 'deep_mastery'  // Deep dive
    }
  }
}
```

---

## Testing Strategy

### Unit Tests

Test each spectrum calculation independently:

```typescript
describe('SophrosyneEngine', () => {
  it('should favor exploration under high uncertainty', () => {
    const engine = new SophrosyneEngine()
    const optimal = engine.calculateOptimal(
      explorationSpectrum,
      {
        ...baseContext,
        uncertainty: 0.9,
        currentPerformance: 0.3
      }
    )
    expect(optimal.position).toBeLessThan(0.4)  // Should explore
  })
  
  it('should favor exploitation under high stakes', () => {
    const optimal = engine.calculateOptimal(
      explorationSpectrum,
      {
        ...baseContext,
        stakes: 0.9,
        uncertainty: 0.3
      }
    )
    expect(optimal.position).toBeGreaterThan(0.6)  // Should exploit
  })
})
```

### Integration Tests

Test regulation decisions in realistic scenarios:

```typescript
it('should recommend persistence when near task completion', async () => {
  const decision = await engine.regulate(
    persistenceSpectrum,
    {
      ...baseContext,
      taskProgress: 0.85,
      currentPerformance: 0.7
    }
  )
  expect(decision.action).toBe('continue')  // Persist to finish
})
```

---

## Metrics & Validation

### Success Metrics

1. **Regulation Quality**: Decisions lead to better outcomes
2. **Learning Rate**: System improves over time
3. **Context Sensitivity**: Different contexts → different optima
4. **Confidence Calibration**: Confidence matches actual optimality

### Validation Approach

1. Record all regulation decisions and outcomes
2. Analyze correlation between following recommendations and success
3. Track improvement in performance over time
4. Compare against baseline (random or fixed strategies)

---

## Implementation Timeline

**Week 1:**
- Core SophrosyneEngine class
- Five main spectra implementations
- Context evaluation logic
- Basic history tracking

**Week 2:**
- Historical learning system
- Confidence calculation
- Integration with agents
- Testing and validation

---

## Philosophy & Wisdom Connection

Sophrosyne is one of the cardinal virtues in ancient Greek philosophy, essential for wisdom:

- **Not static moderation** - context-dependent optimization
- **Self-knowledge** - understanding one's position
- **Dynamic balance** - continuous adjustment
- **Practical wisdom (phronesis)** - knowing what to do in each situation

By implementing sophrosyne, AIRI gains the ability to regulate itself optimally - a key component of wisdom.

---

*"The mean is not a mathematical midpoint, but the contextually optimal point."*

*"Sophrosyne is not about being moderate in all things, but about being excellent at finding the right measure for each situation."*
