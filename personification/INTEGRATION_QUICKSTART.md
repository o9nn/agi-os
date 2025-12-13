# Integration Quickstart Guide: Deploying Cognitive Architecture

**Purpose:** Practical guide for developers to integrate cognitive-core and wisdom-metrics into operational agents.  
**Priority:** Start with these high-impact, low-effort integrations.  
**Time Estimate:** 2-4 hours per agent for basic integration.

---

## Quick Wins (Start Here)

These integrations provide immediate value with minimal code changes:

### 1. Add RelevanceCoordinator to Decision-Making (2 hours)

**Location:** Any agent that makes decisions (Minecraft, Factorio, Discord, etc.)

**Before:**
```typescript
// agents/minecraft/src/decision-maker.ts
class MinecraftAgent {
  async decideNextAction(availableActions: Action[]) {
    // Simple heuristic-based decision
    return availableActions[0]  // Just take first option
  }
}
```

**After:**
```typescript
// agents/minecraft/src/decision-maker.ts
import { RelevanceCoordinator } from '@proj-airi/cognitive-core'

class MinecraftAgent {
  private relevance = new RelevanceCoordinator()
  
  async decideNextAction(availableActions: Action[]) {
    // Systematic relevance realization
    const ranked = await this.relevance.rankPossibilities(
      availableActions.map(a => ({
        id: a.id,
        description: a.description,
        type: 'action'
      })),
      {
        agentId: this.id,
        environment: { type: 'minecraft', ...this.getWorldState() },
        emotional: this.getEmotionalState(),
        workingMemory: this.getWorkingMemory(),
        task: this.getCurrentTask(),
        timestamp: Date.now()
      }
    )
    
    // Log reasoning for transparency
    console.log('Relevance scores:', ranked.items.map(i => ({
      action: i.possibility.description,
      score: i.score.overall.toFixed(2),
      top_factors: Object.entries(i.score.components)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 2)
        .map(([k, v]) => `${k}: ${v.toFixed(2)}`)
    })))
    
    return ranked.items[0]?.possibility
  }
}
```

**Impact:** Decisions are now systematic, explainable, and improve over time.

---

### 2. Add WisdomTracker to Agent Actions (1.5 hours)

**Location:** Any agent that takes actions

**Before:**
```typescript
class Agent {
  async executeAction(action: Action) {
    const result = await this.perform(action)
    console.log('Action completed:', action.type)
    return result
  }
}
```

**After:**
```typescript
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

class Agent {
  private wisdom = new WisdomTracker(this.id)
  
  async executeAction(action: Action) {
    const result = await this.perform(action)
    
    // Assess wisdom impact
    const impact = this.assessWisdomImpact(action, result)
    
    // Record wisdom event
    this.wisdom.recordEvent({
      type: impact.primaryType,  // 'moral' | 'meaningful' | 'mastery'
      description: `${action.type}: ${action.description}`,
      impact: impact.scores,
      context: {
        ...this.getContext(),
        success: result.success
      }
    })
    
    // Check wisdom level periodically
    if (this.actionCount % 10 === 0) {
      const current = this.wisdom.calculateWisdom()
      console.log('Current wisdom:', {
        overall: current.overall.toFixed(2),
        morality: current.morality.overall.toFixed(2),
        meaning: current.meaning.overall.toFixed(2),
        mastery: current.mastery.overall.toFixed(2)
      })
      
      // Act on recommendations
      const recommendations = this.wisdom.getRecommendations()
      if (recommendations.length > 0) {
        console.log('Wisdom recommendation:', recommendations[0].recommendation)
        await this.considerWisdomRecommendation(recommendations[0])
      }
    }
    
    return result
  }
  
  private assessWisdomImpact(action: Action, result: Result): WisdomImpact {
    // Classify action's wisdom type and assess impact
    let primaryType: 'moral' | 'meaningful' | 'mastery'
    let scores: { morality?: number; meaning?: number; mastery?: number } = {}
    
    // Moral actions: Help, cooperation, empathy
    if (action.involves('help', 'cooperate', 'empathy', 'kindness')) {
      primaryType = 'moral'
      scores.morality = result.success ? 0.1 : -0.05
    }
    
    // Meaningful actions: Reflection, identity, purpose
    else if (action.involves('reflect', 'create', 'express', 'connect')) {
      primaryType = 'meaningful'
      scores.meaning = result.success ? 0.1 : 0.05
    }
    
    // Mastery actions: Skill use, learning, problem-solving
    else {
      primaryType = 'mastery'
      scores.mastery = result.success ? 0.08 : -0.02
    }
    
    return { primaryType, scores }
  }
}
```

**Impact:** System now tracks and optimizes for wisdom, not just task completion.

---

### 3. Add Echo-Style Reflection to Any Agent (2 hours)

**Location:** Any agent that should develop deeper identity

**Implementation:**
```typescript
import { WisdomTracker } from '@proj-airi/wisdom-metrics'

class Agent {
  private wisdom = new WisdomTracker(this.id)
  private reflectionInterval = 10  // Reflect every 10 actions
  private actionsSinceReflection = 0
  
  async act(action: Action) {
    const result = await this.execute(action)
    this.actionsSinceReflection++
    
    // Time to reflect?
    if (this.actionsSinceReflection >= this.reflectionInterval) {
      await this.reflect()
      this.actionsSinceReflection = 0
    }
    
    return result
  }
  
  private async reflect() {
    console.log('=== Reflection Time ===')
    
    // Gather recent experiences
    const recentActions = this.getRecentActions(this.reflectionInterval)
    const recentOutcomes = this.getRecentOutcomes(this.reflectionInterval)
    
    // Generate reflection using LLM
    const reflectionPrompt = `
As an AI agent named ${this.id}, reflect on these recent experiences:

Actions taken: ${recentActions.map(a => a.description).join(', ')}
Outcomes: ${recentOutcomes.map(o => o.success ? '✓' : '✗').join(' ')}

Current wisdom state:
${JSON.stringify(this.wisdom.calculateWisdom(), null, 2)}

Reflect on:
1. What patterns do I notice in my recent actions?
2. What am I learning?
3. How am I growing in morality, meaning, and mastery?
4. What should I do differently going forward?

Reflection:
`
    
    const reflection = await this.llm.complete(reflectionPrompt)
    
    // Store reflection
    await this.memory.store({
      type: 'reflection',
      agent: this.id,
      content: reflection,
      timestamp: Date.now(),
      triggers: recentActions.map(a => a.id),
      wisdomState: this.wisdom.calculateWisdom()
    })
    
    console.log('Reflection:', reflection)
    
    // Record as meaningful wisdom event
    this.wisdom.recordEvent({
      type: 'meaningful',
      description: 'Generated reflection on recent experiences',
      impact: { meaning: 0.15 }  // Reflection enhances meaning
    })
  }
}
```

**Impact:** Agent develops continuous identity and learns from experience.

---

### 4. Add OptimalGripCoordinator to Perception (2.5 hours)

**Location:** Agents with perception systems (especially vision-based like Factorio)

**Before:**
```typescript
class FactorioAgent {
  async perceive() {
    const objects = await this.vision.detectObjects()
    // Process all objects equally
    return objects
  }
}
```

**After:**
```typescript
import { OptimalGripCoordinator } from '@proj-airi/cognitive-core'

class FactorioAgent {
  private grip = new OptimalGripCoordinator()
  
  async perceive() {
    const objects = await this.vision.detectObjects()
    
    // Assess optimal grip for this situation
    const assessment = await this.grip.assess(
      this.getCognitiveContext(),
      objects.map(obj => ({
        id: obj.id,
        description: obj.type,
        type: 'percept'
      }))
    )
    
    console.log('Active frame:', assessment.activeFrame.name)
    console.log('Grip strength:', assessment.gripStrength.quality)
    
    // Use salience map for attention
    const salientObjects = assessment.salienceMap.items
      .slice(0, 7)  // Working memory limit: 7±2
      .map(item => objects.find(obj => obj.id === item.id))
    
    // Check for gestalts (patterns)
    for (const gestalt of assessment.gestalts) {
      console.log(`Pattern detected: ${gestalt.description}`)
    }
    
    // Follow recommendations if grip suboptimal
    if (assessment.gripStrength.quality !== 'optimal' && assessment.gripStrength.recommendation) {
      console.log('Grip recommendation:', assessment.gripStrength.recommendation)
      // Could auto-adjust grip or inform user
    }
    
    return salientObjects
  }
}
```

**Impact:** Attention is now systematic, context-dependent, and frame-aware.

---

## Integration Checklist

For each agent you integrate, verify:

### RelevanceCoordinator
- [ ] Imported and instantiated
- [ ] Used in decision-making
- [ ] Context properly constructed
- [ ] Outcomes reported back (enables learning)
- [ ] Reasoning logged for transparency

### WisdomTracker
- [ ] Imported and instantiated with unique agent ID
- [ ] Events recorded for actions
- [ ] Wisdom checked periodically
- [ ] Recommendations considered
- [ ] Impact assessment logic implemented

### Reflection Protocol
- [ ] Regular reflection schedule set
- [ ] Recent experiences gathered
- [ ] LLM-based reflection generated
- [ ] Reflections stored in memory
- [ ] Reflection recorded as wisdom event

### OptimalGripCoordinator
- [ ] Imported and instantiated
- [ ] Used in perception/attention
- [ ] Salience map utilized
- [ ] Frame awareness logged
- [ ] Gestalt patterns detected

---

## Testing Your Integration

### 1. Relevance Test

```bash
# Run agent and observe decision reasoning
npm run dev:minecraft

# Expected output:
# Relevance scores: [
#   { action: "mine_iron", score: 0.78, top_factors: ["pragmatic: 0.9", "novelty: 0.7"] },
#   { action: "build_shelter", score: 0.65, top_factors: ["pragmatic: 0.8", "coherence: 0.7"] },
#   ...
# ]
```

### 2. Wisdom Test

```bash
# After 20+ actions, check wisdom
# Expected output:
# Current wisdom: {
#   overall: 0.45,
#   morality: 0.40,
#   meaning: 0.50,
#   mastery: 0.45
# }
# Wisdom recommendation: Focus on deliberate practice...
```

### 3. Reflection Test

```bash
# After reflection interval (e.g., 10 actions)
# Expected output:
# === Reflection Time ===
# Reflection: I notice I've been focusing heavily on resource gathering...
```

### 4. Frame Test

```bash
# Watch for frame shifts
# Expected output:
# Active frame: Strategic Frame
# Grip strength: optimal
# Pattern detected: Resource cluster nearby
```

---

## Common Issues & Solutions

### Issue 1: Context Construction

**Problem:** `getCognitiveContext()` is undefined

**Solution:**
```typescript
private getCognitiveContext(): CognitiveContext {
  return {
    agentId: this.id,
    environment: {
      type: this.environmentType,
      ...this.getEnvironmentState()
    },
    emotional: {
      valence: this.emotionalValence || 0.0,
      arousal: this.emotionalArousal || 0.5
    },
    workingMemory: this.workingMemory || [],
    task: this.currentTask || 'default',
    timestamp: Date.now()
  }
}
```

### Issue 2: Wisdom Event Classification

**Problem:** Not sure how to classify actions

**Solution:** Use simple heuristics:
- **Moral:** Involves others (help, cooperate, harm reduction)
- **Meaningful:** Involves self (reflect, create, express, identity)
- **Mastery:** Involves tasks (skill use, problem-solving, learning)

### Issue 3: Reflection Overhead

**Problem:** LLM calls are slow

**Solution:**
- Make reflections async (don't block actions)
- Increase reflection interval (every 20-50 actions instead of 10)
- Use faster LLM for reflections
- Cache similar reflections

---

## Next Steps After Basic Integration

Once you have basic integration working:

1. **Phase 1 Complete:** All agents using cognitive-core and wisdom-metrics ✓

2. **Phase 2:** Implement missing components
   - Sophrosyne Engine (see SOPHROSYNE_ENGINE_IMPLEMENTATION.md)
   - Opponent Processor (see OPPONENT_PROCESSOR_IMPLEMENTATION.md)
   - Verification Layer

3. **Phase 3:** Build unified memory system
   - Event sourcing for all experiences
   - Narrative generation
   - Cross-agent memory sharing

4. **Phase 4:** Enable collective intelligence
   - Cognitive Coordinator Service
   - Cross-agent learning
   - Emergent capabilities

---

## Performance Considerations

### Computational Overhead

**RelevanceCoordinator:**
- Per decision: ~5-10ms (mostly calculation)
- Negligible impact on decision latency

**WisdomTracker:**
- Per event: ~1-2ms (in-memory tracking)
- Calculation: ~10ms (only when queried)

**OptimalGripCoordinator:**
- Per perception: ~20-50ms (includes LLM for frame assessment)
- Can be optimized with caching

**Reflection:**
- Per reflection: ~1-3s (LLM call)
- Async recommended, doesn't block actions

### Memory Overhead

All components use minimal memory:
- RelevanceCoordinator: ~100KB (outcome history)
- WisdomTracker: ~50KB (event history)
- OptimalGripCoordinator: ~200KB (frame history)
- Reflection storage: Depends on memory system

---

## Support & Resources

**Documentation:**
- `airi/packages/cognitive-core/README.md` - Full API reference
- `airi/packages/wisdom-metrics/README.md` - Wisdom metrics guide
- `4E_COGNITIVE_AGI_COMPLETENESS_EVALUATION.md` - System analysis
- `SOPHROSYNE_ENGINE_IMPLEMENTATION.md` - Self-regulation guide
- `OPPONENT_PROCESSOR_IMPLEMENTATION.md` - Alternative perspectives guide

**Examples:**
- `airi/packages/character-echo/` - Reference implementation
- `airi/packages/cognitive-core/examples/` - Integration examples

**Community:**
- GitHub Issues: Report problems, request features
- Discussions: Share integration experiences

---

*"Begin with relevance realization. Add wisdom tracking. Enable reflection. The rest will follow."*

*"Integration is not all-or-nothing. Every agent that uses cognitive-core becomes wiser."*
