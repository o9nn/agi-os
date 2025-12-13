# Opponent Processor Implementation Guide

**Component:** Alternative Perspective Generation & Dialectical Reasoning  
**Package:** `@proj-airi/cognitive-core`  
**Priority:** HIGHEST  
**Philosophy:** Truth through dialectical engagement, not confirmation bias

---

## Overview

The Opponent Processor implements **systematic generation of alternative perspectives** to prevent confirmation bias and approach truth through dialectical reasoning. Instead of seeking only supporting evidence, it actively generates the strongest possible counter-arguments.

### Key Principle

> **"Steel-man, don't straw-man: Create the strongest possible version of opposing views."**

Truth emerges not from defending your position against weak attacks, but from engaging with the strongest possible alternatives.

---

## Core Capabilities

### 1. Alternative Frame Generation

**Purpose:** What other ways could I interpret this situation?

**Types of Alternatives:**
- **Opposite perspective**: Direct contrary view
- **Orthogonal perspectives**: Views from different angles
- **Domain transfer**: How would a different field view this?
- **Scale shift**: What changes at different scales (micro/macro)?
- **Temporal shift**: How does time perspective change this?

### 2. Steel-Manning

**Purpose:** Create the strongest possible version of opposing positions.

**Process:**
1. Identify opposing view
2. Find its best arguments
3. Remove weak points
4. Strengthen core logic
5. Present strongest possible case

**Contrast with Straw-Manning:**
- Straw-man: Weak version of opponent's view → easy to defeat → false confidence
- Steel-man: Strong version of opponent's view → hard to defeat → genuine understanding

### 3. Dialectical Synthesis

**Purpose:** Integrate opposing views into higher-order understanding.

**Hegelian Dialectic:**
- **Thesis**: Initial position
- **Antithesis**: Opposing position
- **Synthesis**: Integration that transcends both

**Example:**
- Thesis: "Exploration is best"
- Antithesis: "Exploitation is best"
- Synthesis: "Optimal balance depends on context" (Sophrosyne!)

### 4. Bias Detection

**Purpose:** Identify systematic reasoning errors.

**Common Biases:**
- Confirmation bias: Seeking supporting evidence only
- Availability bias: Overweighting recent/vivid information
- Anchoring bias: Fixating on first information
- Recency bias: Overweighting recent events
- Affect heuristic: Emotion driving reasoning
- Dunning-Kruger: Overconfidence from limited knowledge

---

## Architecture

```typescript
// packages/cognitive-core/src/opponent-processor.ts

export interface Frame {
  perspective: string
  assumptions: string[]
  focus: string
  blindSpots: string[]
  strengths: string[]
  weaknesses: string[]
}

export interface Position {
  claim: string
  arguments: Argument[]
  evidence: Evidence[]
  assumptions: string[]
}

export interface AlternativeFrame {
  frame: Frame
  fitness: number  // How well does this fit context?
  novelty: number  // How different from current frame?
  reasoning: string
}

export interface SteelManResult {
  originalPosition: Position
  strengthenedPosition: Position
  improvements: string[]
  confidence: number
}

export interface Synthesis {
  thesis: Frame
  antithesis: Frame
  synthesis: Frame
  preserves: string[]  // What's retained from both
  transcends: string[]  // What's transcended
  emergent: string[]  // New insights that emerge
}

export interface BiasAssessment {
  biasesDetected: DetectedBias[]
  confidence: number
  recommendations: string[]
}

export class OpponentProcessor {
  private llm: LLMProvider
  private frameLibrary: FrameLibrary
  private biasPatterns: BiasPatternMatcher
  
  /**
   * Generate alternative interpretations of current frame
   */
  async generateAlternatives(
    currentFrame: Frame,
    context: CognitiveContext,
    options: {
      count?: number  // How many alternatives to generate
      minNovelty?: number  // Minimum novelty threshold (0-1)
      types?: AlternativeType[]  // Which types to include
    } = {}
  ): Promise<AlternativeFrame[]> {
    const { 
      count = 3, 
      minNovelty = 0.3,
      types = ['opposite', 'orthogonal', 'domain-transfer']
    } = options
    
    const alternatives: AlternativeFrame[] = []
    
    // Generate opposite perspective
    if (types.includes('opposite')) {
      const opposite = await this.generateOpposite(currentFrame, context)
      if (opposite.novelty >= minNovelty) {
        alternatives.push(opposite)
      }
    }
    
    // Generate orthogonal perspectives
    if (types.includes('orthogonal')) {
      const orthogonal = await this.generateOrthogonal(currentFrame, context, count)
      alternatives.push(...orthogonal.filter(a => a.novelty >= minNovelty))
    }
    
    // Generate domain transfers
    if (types.includes('domain-transfer')) {
      const transfers = await this.generateDomainTransfers(currentFrame, context)
      alternatives.push(...transfers.filter(a => a.novelty >= minNovelty))
    }
    
    // Generate scale shifts
    if (types.includes('scale-shift')) {
      const scaleShifts = await this.generateScaleShifts(currentFrame, context)
      alternatives.push(...scaleShifts)
    }
    
    // Sort by combination of novelty and fitness
    return alternatives
      .sort((a, b) => (b.novelty * 0.6 + b.fitness * 0.4) - (a.novelty * 0.6 + a.fitness * 0.4))
      .slice(0, count)
  }
  
  /**
   * Create strongest possible version of a position (steel-man)
   */
  async steelMan(position: Position, context: CognitiveContext): Promise<SteelManResult> {
    // 1. Identify core arguments
    const coreArguments = this.identifyCore(position.arguments)
    
    // 2. Find supporting evidence
    const additionalEvidence = await this.findSupportingEvidence(position, context)
    
    // 3. Remove weak arguments
    const strongArguments = coreArguments.filter(arg => 
      this.assessArgumentStrength(arg, context) > 0.6
    )
    
    // 4. Strengthen core logic
    const strengthenedArguments = await Promise.all(
      strongArguments.map(arg => this.strengthenArgument(arg, context))
    )
    
    // 5. Address obvious counter-arguments
    const fortifiedArguments = await Promise.all(
      strengthenedArguments.map(arg => this.fortifyAgainstCounters(arg, context))
    )
    
    const strengthened: Position = {
      claim: position.claim,
      arguments: fortifiedArguments,
      evidence: [...position.evidence, ...additionalEvidence],
      assumptions: this.clarifyAssumptions(position.assumptions)
    }
    
    return {
      originalPosition: position,
      strengthenedPosition: strengthened,
      improvements: this.describeImprovements(position, strengthened),
      confidence: this.assessPositionStrength(strengthened, context)
    }
  }
  
  /**
   * Dialectical integration of opposing positions
   */
  async dialecticalSynthesis(
    thesis: Frame,
    antithesis: Frame,
    context: CognitiveContext
  ): Promise<Synthesis> {
    // 1. Find common ground
    const commonGround = this.findCommonGround(thesis, antithesis)
    
    // 2. Identify tensions
    const tensions = this.identifyTensions(thesis, antithesis)
    
    // 3. Find higher-order frame that resolves tensions
    const synthesis = await this.findSynthesis(thesis, antithesis, tensions, context)
    
    // 4. Identify what emerges
    const emergent = this.identifyEmergentInsights(thesis, antithesis, synthesis)
    
    return {
      thesis,
      antithesis,
      synthesis,
      preserves: commonGround,
      transcends: tensions.map(t => t.description),
      emergent
    }
  }
  
  /**
   * Detect reasoning biases
   */
  async detectBiases(
    reasoning: ReasoningChain,
    context: CognitiveContext
  ): Promise<BiasAssessment> {
    const detected: DetectedBias[] = []
    
    // Check for confirmation bias
    const confirmationBias = this.checkConfirmationBias(reasoning)
    if (confirmationBias.present) {
      detected.push(confirmationBias)
    }
    
    // Check for availability bias
    const availabilityBias = this.checkAvailabilityBias(reasoning, context)
    if (availabilityBias.present) {
      detected.push(availabilityBias)
    }
    
    // Check for anchoring bias
    const anchoringBias = this.checkAnchoringBias(reasoning)
    if (anchoringBias.present) {
      detected.push(anchoringBias)
    }
    
    // Check for affect heuristic
    const affectBias = this.checkAffectHeuristic(reasoning, context)
    if (affectBias.present) {
      detected.push(affectBias)
    }
    
    return {
      biasesDetected: detected,
      confidence: this.calculateBiasDetectionConfidence(detected),
      recommendations: this.generateDebiasing Recommendations(detected)
    }
  }
  
  // === Private Implementation Methods ===
  
  private async generateOpposite(
    frame: Frame,
    context: CognitiveContext
  ): Promise<AlternativeFrame> {
    // Flip assumptions
    const oppositeAssumptions = frame.assumptions.map(a => this.negateMeaningfully(a))
    
    // Inverse focus
    const oppositeFocus = this.identifyInverseFocus(frame.focus)
    
    // Generate opposite perspective
    const prompt = this.constructOppositePrompt(frame, context)
    const oppositeDescription = await this.llm.complete(prompt)
    
    return {
      frame: {
        perspective: oppositeDescription,
        assumptions: oppositeAssumptions,
        focus: oppositeFocus,
        blindSpots: frame.strengths,  // What the original saw well
        strengths: frame.blindSpots,  // What the original missed
        weaknesses: this.identifyOppositeWeaknesses(frame)
      },
      fitness: await this.evaluateFitness(oppositeDescription, context),
      novelty: this.calculateNovelty(frame, oppositeDescription),
      reasoning: `Opposite perspective to "${frame.perspective}"`
    }
  }
  
  private async generateOrthogonal(
    frame: Frame,
    context: CognitiveContext,
    count: number
  ): Promise<AlternativeFrame[]> {
    // Generate perspectives that are different but not opposite
    // Examples:
    // - Analytical frame → try emotional frame
    // - Individual frame → try systemic frame
    // - Short-term frame → try long-term frame
    
    const orthogonalDimensions = this.identifyOrthogonalDimensions(frame)
    const alternatives: AlternativeFrame[] = []
    
    for (const dimension of orthogonalDimensions.slice(0, count)) {
      const prompt = this.constructOrthogonalPrompt(frame, dimension, context)
      const description = await this.llm.complete(prompt)
      
      alternatives.push({
        frame: await this.parseFrame(description),
        fitness: await this.evaluateFitness(description, context),
        novelty: this.calculateNovelty(frame, description),
        reasoning: `Orthogonal dimension: ${dimension}`
      })
    }
    
    return alternatives
  }
  
  private async generateDomainTransfers(
    frame: Frame,
    context: CognitiveContext
  ): Promise<AlternativeFrame[]> {
    // How would different fields view this?
    const relevantDomains = this.identifyRelevantDomains(frame, context)
    const transfers: AlternativeFrame[] = []
    
    for (const domain of relevantDomains) {
      const prompt = `How would a ${domain} expert view this situation? ${frame.perspective}`
      const description = await this.llm.complete(prompt)
      
      transfers.push({
        frame: await this.parseFrame(description),
        fitness: await this.evaluateFitness(description, context),
        novelty: this.calculateNovelty(frame, description),
        reasoning: `${domain} domain perspective`
      })
    }
    
    return transfers
  }
  
  private findCommonGround(thesis: Frame, antithesis: Frame): string[] {
    const common: string[] = []
    
    // Find shared assumptions using Set for efficiency
    const antithesisSet = new Set(antithesis.assumptions)
    for (const thesisAssumption of thesis.assumptions) {
      // Direct match or similarity check
      if (antithesisSet.has(thesisAssumption) || 
          antithesis.assumptions.some(a => this.areSimilar(thesisAssumption, a))) {
        common.push(thesisAssumption)
      }
    }
    
    // Find complementary strengths
    for (const thesisStrength of thesis.strengths) {
      if (!antithesis.blindSpots.some(b => this.areSimilar(b, thesisStrength))) {
        common.push(`Thesis insight: ${thesisStrength}`)
      }
    }
    
    return common
  }
  
  private identifyTensions(thesis: Frame, antithesis: Frame): Tension[] {
    const tensions: Tension[] = []
    
    // Find contradictory assumptions
    for (const thesisAssumption of thesis.assumptions) {
      for (const antithesisAssumption of antithesis.assumptions) {
        if (this.areContradictory(thesisAssumption, antithesisAssumption)) {
          tensions.push({
            type: 'contradictory-assumptions',
            thesis: thesisAssumption,
            antithesis: antithesisAssumption,
            description: `Tension between "${thesisAssumption}" and "${antithesisAssumption}"`
          })
        }
      }
    }
    
    // Find incompatible focuses
    if (this.areIncompatible(thesis.focus, antithesis.focus)) {
      tensions.push({
        type: 'incompatible-focus',
        thesis: thesis.focus,
        antithesis: antithesis.focus,
        description: `Cannot simultaneously focus on both "${thesis.focus}" and "${antithesis.focus}"`
      })
    }
    
    return tensions
  }
  
  private async findSynthesis(
    thesis: Frame,
    antithesis: Frame,
    tensions: Tension[],
    context: CognitiveContext
  ): Promise<Frame> {
    // Construct prompt for LLM to help find synthesis
    const prompt = `
Given these two perspectives:
Thesis: ${thesis.perspective}
Antithesis: ${antithesis.perspective}

Tensions to resolve: ${tensions.map(t => t.description).join('; ')}

Find a higher-order frame that:
1. Preserves insights from both perspectives
2. Resolves the tensions
3. Generates new understanding

Synthesis:
`
    
    const synthesisDescription = await this.llm.complete(prompt)
    const synthesisFrame = await this.parseFrame(synthesisDescription)
    
    return synthesisFrame
  }
  
  private checkConfirmationBias(reasoning: ReasoningChain): DetectedBias {
    // Evidence strongly one-sided?
    const supportingEvidence = reasoning.evidence.filter(e => e.supportsConclusion)
    const opposingEvidence = reasoning.evidence.filter(e => !e.supportsConclusion)
    
    const MIN_EVIDENCE_COUNT = 1  // Avoid division by zero
    const ratio = supportingEvidence.length / (opposingEvidence.length || MIN_EVIDENCE_COUNT)
    
    return {
      present: ratio > 5,  // More than 5:1 supporting
      type: 'confirmation',
      severity: Math.min(ratio / 10, 1),
      description: `Evidence is ${ratio.toFixed(1)}:1 supporting conclusion. May be ignoring contrary evidence.`,
      recommendation: 'Actively seek disconfirming evidence and steelman opposing views.'
    }
  }
}
```

---

## Integration Patterns

### Pattern 1: Pre-Decision Opponent Processing

Before making important decisions, generate and consider alternatives:

```typescript
class Agent {
  private opponent = new OpponentProcessor()
  
  async makeImportantDecision(options: Action[]) {
    // Initial analysis
    const initial = await this.analyzeOptions(options)
    const initialFrame = this.getCurrentFrame()
    
    // Generate alternatives
    const alternatives = await this.opponent.generateAlternatives(
      initialFrame,
      this.getContext(),
      { count: 3, minNovelty: 0.4 }
    )
    
    // Re-analyze from each alternative perspective
    const analyses = await Promise.all([
      initial,
      ...alternatives.map(alt => this.analyzeOptionsFromFrame(options, alt.frame))
    ])
    
    // Synthesize insights
    const synthesis = await this.opponent.dialecticalSynthesis(
      initialFrame,
      alternatives[0].frame,  // Best alternative
      this.getContext()
    )
    
    // Make decision informed by multiple perspectives
    return this.decideBasedOnSynthesis(synthesis, analyses)
  }
}
```

### Pattern 2: Continuous Bias Monitoring

Monitor all reasoning for biases:

```typescript
class ReasoningSystem {
  private opponent = new OpponentProcessor()
  
  async reason(premise: Premise, context: CognitiveContext): Promise<Conclusion> {
    // Perform reasoning
    const chain = await this.performReasoning(premise, context)
    
    // Check for biases
    const biases = await this.opponent.detectBiases(chain, context)
    
    if (biases.biasesDetected.length > 0) {
      // Apply debiasing
      const debiased = await this.applyDebiasing(chain, biases)
      return debiased.conclusion
    }
    
    return chain.conclusion
  }
}
```

### Pattern 3: Steel-Manning Before Criticism

Always strengthen opposing views before criticizing:

```typescript
async evaluateProposal(proposal: Proposal): Promise<Evaluation> {
  // Steel-man the proposal first
  const steelManned = await this.opponent.steelMan(
    proposal.position,
    this.getContext()
  )
  
  // Now evaluate the STRONGEST version
  const evaluation = await this.evaluate(steelManned.strengthenedPosition)
  
  return {
    ...evaluation,
    note: 'Evaluated against steel-manned version (strongest possible interpretation)'
  }
}
```

---

## Testing Strategy

```typescript
describe('OpponentProcessor', () => {
  describe('generateAlternatives', () => {
    it('should generate truly opposite perspective', async () => {
      const frame = {
        perspective: 'Exploration is always best',
        assumptions: ['More data is always better', 'Risk is acceptable'],
        focus: 'Discovering new possibilities'
      }
      
      const alternatives = await processor.generateAlternatives(frame, context)
      const opposite = alternatives.find(a => a.reasoning.includes('Opposite'))
      
      expect(opposite.frame.assumptions).toContain('Exploitation is often better')
      expect(opposite.frame.focus).toContain('Utilizing known strategies')
    })
    
    it('should maintain high novelty for alternatives', async () => {
      const alternatives = await processor.generateAlternatives(frame, context, {
        minNovelty: 0.5
      })
      
      alternatives.forEach(alt => {
        expect(alt.novelty).toBeGreaterThanOrEqual(0.5)
      })
    })
  })
  
  describe('steelMan', () => {
    it('should strengthen weak arguments', async () => {
      const weak: Position = {
        claim: 'AI is dangerous',
        arguments: [
          { text: 'AI could become smarter than humans', strength: 0.4 },
          { text: 'Some experts worry about AI', strength: 0.3 }
        ],
        evidence: []
      }
      
      const steelManned = await processor.steelMan(weak, context)
      
      const avgOriginal = weak.arguments.reduce((s, a) => s + a.strength, 0) / weak.arguments.length
      const avgStrengthened = steelManned.strengthenedPosition.arguments
        .reduce((s, a) => s + a.strength, 0) / steelManned.strengthenedPosition.arguments.length
      
      expect(avgStrengthened).toBeGreaterThan(avgOriginal)
    })
  })
  
  describe('dialecticalSynthesis', () => {
    it('should preserve common ground', async () => {
      const thesis = { perspective: 'Explore more', assumptions: ['Learning is valuable'] }
      const antithesis = { perspective: 'Exploit more', assumptions: ['Efficiency is valuable', 'Learning is valuable'] }
      
      const synthesis = await processor.dialecticalSynthesis(thesis, antithesis, context)
      
      expect(synthesis.preserves).toContain('Learning is valuable')
    })
    
    it('should transcend tensions', async () => {
      const synthesis = await processor.dialecticalSynthesis(
        { focus: 'exploration' },
        { focus: 'exploitation' },
        context
      )
      
      expect(synthesis.synthesis.focus).not.toBe('exploration')
      expect(synthesis.synthesis.focus).not.toBe('exploitation')
      expect(synthesis.transcends).toContain('Cannot simultaneously focus on both')
    })
  })
  
  describe('detectBiases', () => {
    it('should detect confirmation bias', async () => {
      const biased: ReasoningChain = {
        conclusion: 'X is true',
        evidence: [
          { supports: true },
          { supports: true },
          { supports: true },
          { supports: true },
          { supports: true },
          { supports: true }
          // No contrary evidence!
        ]
      }
      
      const assessment = await processor.detectBiases(biased, context)
      
      expect(assessment.biasesDetected.some(b => b.type === 'confirmation')).toBe(true)
    })
  })
})
```

---

## Success Metrics

1. **Alternative Quality**: Generated alternatives are genuinely different (novelty > 0.5)
2. **Steel-Manning Strength**: Strengthened positions are measurably stronger
3. **Bias Detection Rate**: Known biased reasoning is detected > 80% of time
4. **Synthesis Quality**: Synthesized frames resolve tensions effectively
5. **Decision Quality**: Decisions using opponent processing have better outcomes

---

## Philosophy & Wisdom Connection

The Opponent Processor embodies several key philosophical principles:

### Socratic Method
Socrates sought truth through systematic questioning and consideration of alternatives, not assertion of beliefs.

### Dialectical Reasoning
Hegel's dialectic: truth emerges through the resolution of contradictions, not the defeat of opponents.

### Intellectual Humility
Recognizing that your first interpretation is likely incomplete or biased.

### Active Open-Mindedness
Systematic consideration of alternatives, not passive acceptance of initial views.

### Truth-Seeking
Orientation toward reality, not toward being right. Willing to change views based on better arguments.

By implementing the Opponent Processor, AIRI gains the ability to approach truth systematically - a key component of wisdom.

---

*"Steel-man, don't straw-man."*

*"Truth emerges through dialectical engagement, not through defeating weak versions of opposing views."*

*"The strongest test of your view is its engagement with the strongest possible alternatives."*
