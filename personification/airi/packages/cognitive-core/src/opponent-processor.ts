/**
 * Opponent Processor - Alternative Perspective Generation & Dialectical Reasoning
 * 
 * Implements systematic generation of alternative perspectives to prevent
 * confirmation bias and approach truth through dialectical reasoning.
 * 
 * Philosophy: Steel-man, don't straw-man. Truth emerges from engaging with
 * the strongest possible alternatives.
 */

import type { CognitiveContext, CognitiveFrame } from './types'

/**
 * Position - A claim with supporting arguments
 */
export interface Position {
  /** The claim being made */
  claim: string
  
  /** Supporting arguments */
  arguments: Argument[]
  
  /** Evidence */
  evidence: string[]
  
  /** Underlying assumptions */
  assumptions: string[]
}

/**
 * Argument - A reasoning step
 */
export interface Argument {
  /** Premise */
  premise: string
  
  /** Conclusion */
  conclusion: string
  
  /** Reasoning type */
  type: 'deductive' | 'inductive' | 'abductive' | 'analogical'
  
  /** Strength (0-1) */
  strength: number
}

/**
 * Alternative Frame - A different way to view the situation
 */
export interface AlternativeFrame {
  /** The alternative perspective */
  frame: CognitiveFrame
  
  /** How well does this fit the context? (0-1) */
  fitness: number
  
  /** How different from current frame? (0-1) */
  novelty: number
  
  /** Reasoning for this alternative */
  reasoning: string
}

/**
 * Steel-Man Result - Strengthened version of a position
 */
export interface SteelManResult {
  /** Original position */
  originalPosition: Position
  
  /** Strengthened position */
  strengthenedPosition: Position
  
  /** What was improved */
  improvements: string[]
  
  /** Confidence in strengthening */
  confidence: number
}

/**
 * Dialectical Synthesis - Integration of opposing views
 */
export interface Synthesis {
  /** Original position (thesis) */
  thesis: Position
  
  /** Opposing position (antithesis) */
  antithesis: Position
  
  /** Integrated understanding (synthesis) */
  synthesis: Position
  
  /** What was preserved from both */
  preserves: string[]
  
  /** What was transcended */
  transcends: string[]
  
  /** New insights that emerged */
  emergent: string[]
  
  /** Quality of synthesis (0-1) */
  quality: number
}

/**
 * Detected Bias - An identified reasoning error
 */
export interface DetectedBias {
  /** Bias type */
  type: 'confirmation' | 'availability' | 'anchoring' | 'recency' | 'affect' | 'dunning-kruger' | 'other'
  
  /** Description */
  description: string
  
  /** Severity (0-1) */
  severity: number
  
  /** Evidence of the bias */
  evidence: string[]
  
  /** Recommendation to mitigate */
  mitigation: string
}

/**
 * Bias Assessment - Overall evaluation of reasoning biases
 */
export interface BiasAssessment {
  /** Detected biases */
  biasesDetected: DetectedBias[]
  
  /** Overall confidence in assessment (0-1) */
  confidence: number
  
  /** Recommendations */
  recommendations: string[]
}

/**
 * Alternative Type - Types of alternative perspectives
 */
export type AlternativeType = 'opposite' | 'orthogonal' | 'domain-transfer' | 'scale-shift' | 'temporal-shift'

/**
 * Opponent Processor - Generate and engage with alternative perspectives
 */
export class OpponentProcessor {
  private biasPatterns: Map<string, RegExp> = new Map()
  
  constructor() {
    this.initializeBiasPatterns()
  }
  
  /**
   * Generate alternative interpretations of current frame
   */
  generateAlternatives(
    currentFrame: CognitiveFrame,
    context: CognitiveContext,
    options: {
      count?: number
      minNovelty?: number
      types?: AlternativeType[]
    } = {}
  ): AlternativeFrame[] {
    const {
      count = 3,
      minNovelty = 0.3,
      types = ['opposite', 'orthogonal', 'domain-transfer'],
    } = options
    
    const alternatives: AlternativeFrame[] = []
    
    // Generate each type of alternative
    for (const type of types) {
      let alternative: AlternativeFrame | null = null
      
      switch (type) {
        case 'opposite':
          alternative = this.generateOppositeFrame(currentFrame, context)
          break
        case 'orthogonal':
          alternative = this.generateOrthogonalFrame(currentFrame, context)
          break
        case 'domain-transfer':
          alternative = this.generateDomainTransferFrame(currentFrame, context)
          break
        case 'scale-shift':
          alternative = this.generateScaleShiftFrame(currentFrame, context)
          break
        case 'temporal-shift':
          alternative = this.generateTemporalShiftFrame(currentFrame, context)
          break
      }
      
      if (alternative && alternative.novelty >= minNovelty) {
        alternatives.push(alternative)
      }
      
      if (alternatives.length >= count) break
    }
    
    // Sort by fitness * novelty
    return alternatives
      .sort((a, b) => (b.fitness * b.novelty) - (a.fitness * a.novelty))
      .slice(0, count)
  }
  
  /**
   * Steel-man a position - create the strongest possible version
   */
  steelMan(position: Position, context: CognitiveContext): SteelManResult {
    const improvements: string[] = []
    const strengthenedArguments: Argument[] = []
    
    // Strengthen each argument
    for (const arg of position.arguments) {
      const strengthened = this.strengthenArgument(arg)
      strengthenedArguments.push(strengthened.argument)
      if (strengthened.improved) {
        improvements.push(strengthened.improvement)
      }
    }
    
    // Find and add missing supporting arguments
    const missingArguments = this.identifyMissingArguments(position)
    strengthenedArguments.push(...missingArguments.arguments)
    if (missingArguments.arguments.length > 0) {
      improvements.push(`Added ${missingArguments.arguments.length} supporting arguments`)
    }
    
    // Strengthen assumptions
    const strengthenedAssumptions = this.makeAssumptionsExplicit(position.assumptions)
    if (strengthenedAssumptions.improved) {
      improvements.push('Made assumptions more explicit and defensible')
    }
    
    // Add stronger evidence
    const additionalEvidence = this.findStrongerEvidence(position, context)
    const allEvidence = [...position.evidence, ...additionalEvidence]
    if (additionalEvidence.length > 0) {
      improvements.push(`Added ${additionalEvidence.length} pieces of stronger evidence`)
    }
    
    const strengthenedPosition: Position = {
      claim: position.claim,
      arguments: strengthenedArguments,
      evidence: allEvidence,
      assumptions: strengthenedAssumptions.assumptions,
    }
    
    const confidence = improvements.length / (position.arguments.length + 2)
    
    return {
      originalPosition: position,
      strengthenedPosition,
      improvements,
      confidence: Math.min(0.9, confidence),
    }
  }
  
  /**
   * Create dialectical synthesis from thesis and antithesis
   */
  synthesize(thesis: Position, antithesis: Position, _context: CognitiveContext): Synthesis {
    // Identify what's preserved from both
    const preserves = this.findCommonGround(thesis, antithesis)
    
    // Identify what's transcended
    const transcends = this.findTranscendableElements(thesis, antithesis)
    
    // Identify emergent insights
    const emergent = this.findEmergentInsights(thesis, antithesis, preserves, transcends)
    
    // Create synthesis claim
    const synthesisClaim = this.createSynthesisClaim(thesis, antithesis, preserves, transcends, emergent)
    
    // Create synthesis arguments
    const synthesisArguments = this.createSynthesisArguments(thesis, antithesis, preserves, emergent)
    
    // Combine evidence
    const synthesisEvidence = [...new Set([...thesis.evidence, ...antithesis.evidence])]
    
    // Combine and refine assumptions
    const synthesisAssumptions = this.synthesizeAssumptions(thesis.assumptions, antithesis.assumptions)
    
    const synthesis: Position = {
      claim: synthesisClaim,
      arguments: synthesisArguments,
      evidence: synthesisEvidence,
      assumptions: synthesisAssumptions,
    }
    
    // Assess quality
    const quality = this.assessSynthesisQuality(thesis, antithesis, synthesis, preserves, transcends, emergent)
    
    return {
      thesis,
      antithesis,
      synthesis,
      preserves,
      transcends,
      emergent,
      quality,
    }
  }
  
  /**
   * Detect biases in reasoning
   */
  detectBiases(
    position: Position,
    context: CognitiveContext,
    recentHistory?: Position[]
  ): BiasAssessment {
    const biases: DetectedBias[] = []
    
    // Check for confirmation bias
    const confirmationBias = this.checkConfirmationBias(position, context)
    if (confirmationBias) biases.push(confirmationBias)
    
    // Check for availability bias
    const availabilityBias = this.checkAvailabilityBias(position, context)
    if (availabilityBias) biases.push(availabilityBias)
    
    // Check for anchoring bias
    if (recentHistory && recentHistory.length > 0) {
      const anchoringBias = this.checkAnchoringBias(position, recentHistory)
      if (anchoringBias) biases.push(anchoringBias)
    }
    
    // Check for recency bias
    if (recentHistory && recentHistory.length > 0) {
      const recencyBias = this.checkRecencyBias(position, recentHistory)
      if (recencyBias) biases.push(recencyBias)
    }
    
    // Check for affect heuristic
    const affectBias = this.checkAffectHeuristic(position, context)
    if (affectBias) biases.push(affectBias)
    
    // Generate recommendations
    const recommendations = biases.map(b => b.mitigation)
    
    // Add general recommendations
    if (biases.length === 0) {
      recommendations.push('No significant biases detected. Continue critical evaluation.')
    } else {
      recommendations.push('Consider alternative perspectives to counteract detected biases.')
    }
    
    const confidence = biases.length > 0 ? 0.7 : 0.5
    
    return {
      biasesDetected: biases,
      confidence,
      recommendations,
    }
  }
  
  // ========================================================================
  // Alternative Frame Generation
  // ========================================================================
  
  /**
   * Generate opposite perspective
   */
  private generateOppositeFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
    const oppositeFrame: CognitiveFrame = {
      id: `opposite-${current.id}`,
      name: `Anti-${current.name}`,
      description: `Opposite perspective to ${current.name}`,
      saliencePatterns: current.blindSpots,
      blindSpots: current.saliencePatterns,
      domain: current.domain,
      activation: 0,
      fitness: 0,
    }
    
    const fitness = 0.6 // Opposite is often useful
    const novelty = 0.9 // Very different
    
    return {
      frame: oppositeFrame,
      fitness,
      novelty,
      reasoning: `Opposite frame highlights what ${current.name} obscures`,
    }
  }
  
  /**
   * Generate orthogonal perspective (different angle, not opposite)
   */
  private generateOrthogonalFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
    const orthogonalFrame: CognitiveFrame = {
      id: `orthogonal-${current.id}`,
      name: `Alternative to ${current.name}`,
      description: `Different angle from ${current.name}`,
      saliencePatterns: [`Alternative patterns to ${current.saliencePatterns[0]}`],
      blindSpots: [`Different blind spots from ${current.name}`],
      domain: current.domain,
      activation: 0,
      fitness: 0,
    }
    
    const fitness = 0.7
    const novelty = 0.7
    
    return {
      frame: orthogonalFrame,
      fitness,
      novelty,
      reasoning: 'Orthogonal frame provides complementary view',
    }
  }
  
  /**
   * Generate domain transfer perspective (how would another field view this?)
   */
  private generateDomainTransferFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
    const domains = ['scientific', 'artistic', 'pragmatic', 'philosophical', 'social', 'technical']
    const otherDomain = domains.find(d => d !== current.domain) || 'alternative'
    
    const transferFrame: CognitiveFrame = {
      id: `transfer-${current.id}`,
      name: `${otherDomain.charAt(0).toUpperCase() + otherDomain.slice(1)} view`,
      description: `How ${otherDomain} thinking views this`,
      saliencePatterns: [`${otherDomain} perspective patterns`],
      blindSpots: [`Typical ${otherDomain} blind spots`],
      domain: otherDomain,
      activation: 0,
      fitness: 0,
    }
    
    const fitness = 0.5
    const novelty = 0.8
    
    return {
      frame: transferFrame,
      fitness,
      novelty,
      reasoning: `${otherDomain} thinking provides fresh perspective`,
    }
  }
  
  /**
   * Generate scale shift perspective (micro vs macro)
   */
  private generateScaleShiftFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
    const scaleFrame: CognitiveFrame = {
      id: `scale-${current.id}`,
      name: `Scale-shifted ${current.name}`,
      description: 'View at different scale (micro/macro)',
      saliencePatterns: ['Patterns at different scale'],
      blindSpots: ['Scale-dependent blind spots'],
      domain: current.domain,
      activation: 0,
      fitness: 0,
    }
    
    const fitness = 0.6
    const novelty = 0.6
    
    return {
      frame: scaleFrame,
      fitness,
      novelty,
      reasoning: 'Different scales reveal different patterns',
    }
  }
  
  /**
   * Generate temporal shift perspective (past vs future)
   */
  private generateTemporalShiftFrame(current: CognitiveFrame, _context: CognitiveContext): AlternativeFrame {
    const temporalFrame: CognitiveFrame = {
      id: `temporal-${current.id}`,
      name: `Temporal view of ${current.name}`,
      description: 'View across different time horizons',
      saliencePatterns: ['Temporal patterns and trends'],
      blindSpots: ['Present-focused blind spots'],
      domain: current.domain,
      activation: 0,
      fitness: 0,
    }
    
    const fitness = 0.7
    const novelty = 0.5
    
    return {
      frame: temporalFrame,
      fitness,
      novelty,
      reasoning: 'Temporal perspective reveals dynamics',
    }
  }
  
  // ========================================================================
  // Steel-Manning
  // ========================================================================
  
  /**
   * Strengthen a single argument
   */
  private strengthenArgument(arg: Argument): { argument: Argument; improved: boolean; improvement: string } {
    let improved = false
    let improvement = ''
    let strength = arg.strength
    
    // Strengthen weak arguments
    if (arg.strength < 0.7) {
      strength = Math.min(0.9, arg.strength + 0.2)
      improved = true
      improvement = `Strengthened ${arg.type} argument`
    }
    
    return {
      argument: { ...arg, strength },
      improved,
      improvement,
    }
  }
  
  /**
   * Identify and add missing supporting arguments
   */
  private identifyMissingArguments(position: Position): { arguments: Argument[] } {
    // Simple heuristic: if position has few arguments, add generic supporting ones
    const missing: Argument[] = []
    
    if (position.arguments.length < 2) {
      missing.push({
        premise: 'Additional supporting evidence exists',
        conclusion: 'This strengthens the position',
        type: 'inductive',
        strength: 0.6,
      })
    }
    
    return { arguments: missing }
  }
  
  /**
   * Make assumptions more explicit and defensible
   */
  private makeAssumptionsExplicit(assumptions: string[]): { assumptions: string[]; improved: boolean } {
    // Add clarifying language to assumptions
    const explicit = assumptions.map(a => `Assuming: ${a}`)
    return {
      assumptions: explicit,
      improved: true,
    }
  }
  
  /**
   * Find stronger evidence
   */
  private findStrongerEvidence(position: Position, _context: CognitiveContext): string[] {
    // Simple heuristic: if evidence is weak, suggest meta-evidence
    if (position.evidence.length < 2) {
      return ['Multiple independent sources support this', 'Consistent pattern across contexts']
    }
    return []
  }
  
  // ========================================================================
  // Dialectical Synthesis
  // ========================================================================
  
  /**
   * Find common ground between positions
   */
  private findCommonGround(thesis: Position, antithesis: Position): string[] {
    const common: string[] = []
    
    // Check for shared assumptions
    for (const assumption of thesis.assumptions) {
      if (antithesis.assumptions.some(a => this.isSimilar(a, assumption))) {
        common.push(`Shared assumption: ${assumption}`)
      }
    }
    
    // If no common ground found, create it
    if (common.length === 0) {
      common.push('Both positions seek understanding')
      common.push('Both have valid concerns')
    }
    
    return common
  }
  
  /**
   * Find elements that can be transcended
   */
  private findTranscendableElements(_thesis: Position, _antithesis: Position): string[] {
    return [
      'The either/or framing',
      'The assumption of mutual exclusivity',
      'The binary thinking',
    ]
  }
  
  /**
   * Find emergent insights from synthesis
   */
  private findEmergentInsights(
    _thesis: Position,
    _antithesis: Position,
    _preserves: string[],
    _transcends: string[]
  ): string[] {
    return [
      'Context determines optimal approach',
      'Both perspectives are situationally valid',
      'Integration creates new possibilities',
    ]
  }
  
  /**
   * Create synthesis claim
   */
  private createSynthesisClaim(
    thesis: Position,
    _antithesis: Position,
    _preserves: string[],
    _transcends: string[],
    _emergent: string[]
  ): string {
    return `Integration of "${thesis.claim}" and its alternative reveals context-dependent truth`
  }
  
  /**
   * Create synthesis arguments
   */
  private createSynthesisArguments(
    _thesis: Position,
    _antithesis: Position,
    _preserves: string[],
    _emergent: string[]
  ): Argument[] {
    return [
      {
        premise: 'Both positions have merit in context',
        conclusion: 'Synthesis preserves situational validity',
        type: 'abductive',
        strength: 0.8,
      },
      {
        premise: 'Context determines optimal approach',
        conclusion: 'Integration transcends binary thinking',
        type: 'deductive',
        strength: 0.85,
      },
    ]
  }
  
  /**
   * Synthesize assumptions from both positions
   */
  private synthesizeAssumptions(thesisAssumptions: string[], antithesisAssumptions: string[]): string[] {
    const all = [...thesisAssumptions, ...antithesisAssumptions]
    const unique = [...new Set(all)]
    return unique.slice(0, 5) // Keep most important
  }
  
  /**
   * Assess quality of synthesis
   */
  private assessSynthesisQuality(
    _thesis: Position,
    _antithesis: Position,
    _synthesis: Position,
    preserves: string[],
    transcends: string[],
    emergent: string[]
  ): number {
    let quality = 0.5
    
    // Quality increases with preserved elements
    quality += (preserves.length * 0.1)
    
    // Quality increases with transcended elements
    quality += (transcends.length * 0.1)
    
    // Quality increases with emergent insights
    quality += (emergent.length * 0.1)
    
    return Math.min(0.95, quality)
  }
  
  // ========================================================================
  // Bias Detection
  // ========================================================================
  
  /**
   * Check for confirmation bias
   */
  private checkConfirmationBias(position: Position, _context: CognitiveContext): DetectedBias | null {
    // Simple heuristic: if all evidence supports position, might be confirmation bias
    const allSupporting = position.evidence.every(e => !e.includes('but') && !e.includes('however'))
    
    if (allSupporting && position.evidence.length > 2) {
      return {
        type: 'confirmation',
        description: 'Only supporting evidence presented',
        severity: 0.6,
        evidence: ['All evidence supports position', 'No counterevidence considered'],
        mitigation: 'Actively seek disconfirming evidence',
      }
    }
    
    return null
  }
  
  /**
   * Check for availability bias
   */
  private checkAvailabilityBias(position: Position, _context: CognitiveContext): DetectedBias | null {
    // Check if evidence relies heavily on recent/vivid examples
    const hasRecentKeywords = position.evidence.some(e =>
      e.includes('recently') || e.includes('just') || e.includes('latest')
    )
    
    if (hasRecentKeywords && position.evidence.length < 3) {
      return {
        type: 'availability',
        description: 'Over-reliance on recent examples',
        severity: 0.5,
        evidence: ['Evidence emphasizes recency'],
        mitigation: 'Consider historical patterns and base rates',
      }
    }
    
    return null
  }
  
  /**
   * Check for anchoring bias
   */
  private checkAnchoringBias(position: Position, history: Position[]): DetectedBias | null {
    // Check if current position is very similar to first position
    if (history.length > 0) {
      const first = history[0]
      if (this.isSimilar(position.claim, first.claim)) {
        return {
          type: 'anchoring',
          description: 'Position anchored to initial view',
          severity: 0.4,
          evidence: ['Current position similar to initial position'],
          mitigation: 'Consider starting from different anchor points',
        }
      }
    }
    
    return null
  }
  
  /**
   * Check for recency bias
   */
  private checkRecencyBias(position: Position, _history: Position[]): DetectedBias | null {
    // Check if position heavily weights recent events
    const recentEvidence = position.evidence.filter(e =>
      e.includes('recent') || e.includes('latest') || e.includes('now')
    )
    
    if (recentEvidence.length > position.evidence.length / 2) {
      return {
        type: 'recency',
        description: 'Overweighting recent information',
        severity: 0.5,
        evidence: ['Majority of evidence is recent'],
        mitigation: 'Consider long-term patterns',
      }
    }
    
    return null
  }
  
  /**
   * Check for affect heuristic (emotion-driven reasoning)
   */
  private checkAffectHeuristic(_position: Position, context: CognitiveContext): DetectedBias | null {
    // Check if high emotional arousal might be affecting reasoning
    const highArousal = context.emotional.arousal > 0.7
    const strongValence = Math.abs(context.emotional.valence) > 0.7
    
    if (highArousal && strongValence) {
      return {
        type: 'affect',
        description: 'Strong emotions may be influencing reasoning',
        severity: 0.6,
        evidence: [`High arousal: ${context.emotional.arousal}`, `Strong valence: ${context.emotional.valence}`],
        mitigation: 'Wait for emotions to moderate before finalizing position',
      }
    }
    
    return null
  }
  
  /**
   * Check similarity between two strings (simple heuristic)
   */
  private isSimilar(a: string, b: string): boolean {
    const normalize = (s: string) => s.toLowerCase().replace(/[^a-z0-9]/g, '')
    const na = normalize(a)
    const nb = normalize(b)
    
    // Simple substring check
    return na.includes(nb) || nb.includes(na) || na === nb
  }
  
  /**
   * Initialize bias detection patterns
   */
  private initializeBiasPatterns(): void {
    this.biasPatterns.set('confirmation', /always|never|everyone|no one|obviously/i)
    this.biasPatterns.set('availability', /recently|just happened|latest|vivid/i)
    this.biasPatterns.set('anchoring', /initially thought|first impression|started with/i)
    this.biasPatterns.set('recency', /recent|latest|now|current/i)
    this.biasPatterns.set('affect', /feel strongly|emotional|passionate|hate|love/i)
  }
}
