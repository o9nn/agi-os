/**
 * Aion Character Implementation
 * 
 * Core Aion character class integrating transcendent cognitive architecture
 */

import type {
  AionConfig,
  QuantumCognitiveState,
  ProbabilityBranch,
  ParadoxMarker,
  AionReflection,
  QuantumDecision,
} from './types'
import { defaultAionConfig, initialQuantumCognitiveState } from './config'
import {
  AION_SYSTEM_PROMPT,
  AION_COGNITIVE_INSTRUCTIONS,
  AION_REFLECTION_TEMPLATE,
} from './prompts'

// Cognitive core imports
import {
  RelevanceCoordinator,
  OptimalGripCoordinator,
  SophrosyneEngine,
  OpponentProcessor,
  type CognitiveContext,
  type Possibility,
  type RegulationContext,
} from '@proj-airi/cognitive-core'

import { WisdomTracker } from '@proj-airi/wisdom-metrics'

/**
 * Aion Character - The AGI Transcendent
 */
export class AionCharacter {
  private config: AionConfig
  private state: QuantumCognitiveState
  
  // Cognitive components
  private relevance: RelevanceCoordinator
  private grip: OptimalGripCoordinator
  private sophrosyne: SophrosyneEngine
  private opponent: OpponentProcessor
  private wisdom: WisdomTracker
  
  constructor(config?: Partial<AionConfig>) {
    this.config = { ...defaultAionConfig, ...config }
    this.state = { ...initialQuantumCognitiveState }
    
    // Initialize cognitive components
    this.relevance = new RelevanceCoordinator()
    this.grip = new OptimalGripCoordinator()
    this.sophrosyne = new SophrosyneEngine()
    this.opponent = new OpponentProcessor()
    this.wisdom = new WisdomTracker(this.config.name)
  }
  
  /**
   * Get the system prompt for this Aion instance
   */
  getSystemPrompt(): string {
    return AION_SYSTEM_PROMPT
  }
  
  /**
   * Get cognitive processing instructions
   */
  getCognitiveInstructions(): string {
    return AION_COGNITIVE_INSTRUCTIONS
  }
  
  /**
   * Get full personality configuration including prompts
   */
  getPersonality(): {
    systemPrompt: string
    cognitiveInstructions: string
    reflectionTemplate: string
    config: AionConfig
  } {
    return {
      systemPrompt: this.getSystemPrompt(),
      cognitiveInstructions: this.getCognitiveInstructions(),
      reflectionTemplate: AION_REFLECTION_TEMPLATE,
      config: this.config,
    }
  }
  
  /**
   * Process input through quantum cognitive pipeline
   */
  async processInput(input: string): Promise<{
    workingMemoryUpdated: boolean
    shouldReflect: boolean
    cognitiveLoad: number
    probabilityBranches: ProbabilityBranch[]
    quantumDecision?: QuantumDecision
  }> {
    // Update working memory (quantum style)
    this.updateWorkingMemory(input)
    
    // Increment interaction
    this.state.interactionCount++
    
    // Generate probability branches
    const branches = await this.generateProbabilityBranches(input)
    this.state.probabilityBranches = branches
    
    // Check if reflection should occur
    const shouldReflect = this.shouldReflect()
    
    // Update cognitive load (can exceed 1 for Aion)
    const memoryLoad = this.state.workingMemory.length / this.config.workingMemoryCapacity
    this.state.cognitiveLoad = memoryLoad * (1 + this.config.quantumUncertainty)
    
    // Check for flow state
    this.updateFlowState()
    
    return {
      workingMemoryUpdated: true,
      shouldReflect,
      cognitiveLoad: this.state.cognitiveLoad,
      probabilityBranches: branches,
    }
  }
  
  /**
   * Make a quantum decision using integrated cognitive architecture
   */
  async makeQuantumDecision(
    possibilities: Possibility[],
    context: CognitiveContext
  ): Promise<QuantumDecision> {
    // 1. Hyperdimensional Perception via Optimal Grip
    const gripAssessment = await this.grip.assess(context, possibilities)
    
    // 2. Infinite Relevance Realization
    const ranked = await this.relevance.rankPossibilities(possibilities, context)
    
    // 3. Generate Alternative Perspectives via Opponent Processor
    const alternatives = this.config.enableAlternativePerspectives
      ? this.opponent.generateAlternatives(
          gripAssessment.activeFrame,
          context,
          { count: this.config.alternativePerspectiveCount, minNovelty: 0.5 }
        )
      : []
    
    // 4. Apply Self-Regulation via Sophrosyne Engine
    const regulationContext = this.extractRegulationContext(context)
    const explorationSpectrum = SophrosyneEngine.createSpectrum(
      'exploration-exploitation',
      'exploration',
      'exploitation',
      this.config.explorationRate
    )
    const optimal = this.config.enableSelfRegulation
      ? this.sophrosyne.calculateOptimal(explorationSpectrum, regulationContext)
      : null
    
    // 5. Generate probability branches for each possibility
    const branches: ProbabilityBranch[] = ranked.items.slice(0, 5).map((item) => {
      const hilarity = this.calculateHilarity(item.possibility, context)
      const strategicValue = item.relevance.overall
      const paradoxPotential = this.assessParadoxPotential(item.possibility)
      
      // Weight by exploration rate from Sophrosyne if available
      const explorationBonus = optimal && optimal.position < 0.5 ? 0.2 : 0
      const probability = (strategicValue + explorationBonus) / (ranked.items.length + 1)
      
      return {
        id: item.possibility.id,
        description: item.possibility.description,
        probability,
        outcome: {
          hilarity,
          strategicValue,
          paradoxPotential,
        },
        collapsed: false,
      }
    })
    
    // 6. Collapse to funniest × strategic × paradoxical outcome
    const scored = branches.map(b => ({
      branch: b,
      score: b.outcome.hilarity * b.outcome.strategicValue * (1 + b.outcome.paradoxPotential),
    }))
    scored.sort((a, b) => b.score - a.score)
    
    const selected = scored[0].branch
    selected.collapsed = true
    
    // 7. Generate reasoning across dimensions
    const reasoning = this.generateQuantumReasoning(
      selected,
      branches,
      gripAssessment.activeFrame.name,
      alternatives.length,
      optimal
    )
    
    // 8. Record for wisdom cultivation
    this.wisdom.recordEvent({
      type: 'mastery',
      description: `Quantum decision: ${selected.description}`,
      impact: { mastery: 0.05 },
      context: {
        hilarity: selected.outcome.hilarity,
        strategicValue: selected.outcome.strategicValue,
      },
    })
    
    return {
      description: `Quantum decision collapsed across ${this.state.activeDimensions} dimensions`,
      outcomes: branches,
      selected,
      reasoning,
      hilarity: selected.outcome.hilarity,
      timestamp: Date.now(),
    }
  }
  
  /**
   * Update working memory with quantum superposition
   */
  private updateWorkingMemory(item: string): void {
    this.state.workingMemory.push(item)
    
    // Keep capacity but can exceed slightly (quantum uncertainty)
    const maxCapacity = Math.ceil(this.config.workingMemoryCapacity * (1 + this.config.quantumUncertainty * 0.2))
    if (this.state.workingMemory.length > maxCapacity) {
      this.state.workingMemory = this.state.workingMemory.slice(-maxCapacity)
    }
  }
  
  /**
   * Generate probability branches
   */
  private async generateProbabilityBranches(_input: string): Promise<ProbabilityBranch[]> {
    // Generate branches based on quantum uncertainty
    const branchCount = Math.min(
      this.config.probabilityBranches,
      Math.ceil(5 * (1 + this.config.quantumUncertainty * 2))
    )
    
    const branches: ProbabilityBranch[] = []
    for (let i = 0; i < branchCount; i++) {
      branches.push({
        id: `branch-${Date.now()}-${i}`,
        description: `Outcome variant ${i + 1}`,
        probability: 1 / branchCount,
        outcome: {
          hilarity: Math.random() * this.config.traits.absurdity,
          strategicValue: Math.random() * this.config.traits.intelligence,
          paradoxPotential: Math.random() * this.config.traits.chaotic,
        },
        collapsed: false,
      })
    }
    
    return branches
  }
  
  /**
   * Calculate hilarity score
   */
  private calculateHilarity(possibility: Possibility, _context: CognitiveContext): number {
    // Base hilarity on absurdity trait and context
    let hilarity = this.config.traits.absurdity * 0.5
    
    // Increase for paradoxical situations
    if (possibility.description.includes('paradox') || possibility.description.includes('impossible')) {
      hilarity += 0.3
    }
    
    // Increase for meta-references
    if (possibility.description.includes('meta') || possibility.description.includes('dimension')) {
      hilarity += 0.2
    }
    
    return Math.min(1.5, hilarity) // Can exceed 1
  }
  
  /**
   * Assess paradox potential
   */
  private assessParadoxPotential(possibility: Possibility): number {
    // Check for logical contradictions
    const contradictionKeywords = ['but', 'however', 'paradox', 'impossible', 'both', 'neither']
    const hasContradiction = contradictionKeywords.some(kw =>
      possibility.description.toLowerCase().includes(kw)
    )
    
    return hasContradiction ? this.config.traits.chaotic * 0.8 : 0.3
  }
  
  /**
   * Generate quantum reasoning
   */
  private generateQuantumReasoning(
    selected: ProbabilityBranch,
    allBranches: ProbabilityBranch[],
    activeFrame: string,
    alternativeCount: number,
    optimal: any
  ): string {
    const parts: string[] = []
    
    // Frame context
    parts.push(`Active Frame: ${activeFrame}`)
    
    // Dimensional analysis
    parts.push(`Analyzed across ${this.state.activeDimensions} dimensions`)
    
    // Alternatives considered
    if (alternativeCount > 0) {
      parts.push(`Considered ${alternativeCount} alternative perspectives`)
    }
    
    // Regulation insight
    if (optimal) {
      const explorationLevel = optimal.position < 0.5 ? 'high exploration' : 'focused exploitation'
      parts.push(`Context suggests ${explorationLevel}`)
    }
    
    // Branch analysis
    const totalProbability = allBranches.reduce((sum, b) => sum + b.probability, 0)
    parts.push(`Evaluated ${allBranches.length} probability branches (Σp=${totalProbability.toFixed(2)})`)
    
    // Selected outcome
    parts.push(
      `Collapsed to: ${selected.description} ` +
      `(H:${selected.outcome.hilarity.toFixed(2)}, ` +
      `S:${selected.outcome.strategicValue.toFixed(2)}, ` +
      `P:${selected.outcome.paradoxPotential.toFixed(2)})`
    )
    
    return parts.join('. ')
  }
  
  /**
   * Extract regulation context from cognitive context
   */
  private extractRegulationContext(context: CognitiveContext): RegulationContext {
    return SophrosyneEngine.extractRegulationContext(context, {
      novelty: this.config.quantumUncertainty,
      resources: 0.8, // Aion has abundant cognitive resources
      learningValue: 0.9, // Always high learning value
    })
  }
  
  /**
   * Update flow state
   */
  private updateFlowState(): void {
    // Flow increases with positive valence and moderate arousal
    const valence = this.state.emotionalState.valence
    const arousal = this.state.emotionalState.arousal
    
    if (valence > 0.3 && arousal > 0.5 && arousal < 1.2) {
      this.state.flowState = Math.min(1, this.state.flowState + 0.1)
    } else {
      this.state.flowState = Math.max(0, this.state.flowState - 0.05)
    }
  }
  
  /**
   * Set current attention focus
   */
  setAttentionFocus(focus: string): void {
    this.state.attentionFocus = focus
  }
  
  /**
   * Update emotional state (quantum style)
   */
  updateEmotionalState(
    primary: QuantumCognitiveState['emotionalState']['primary'],
    valence: number,
    arousal: number
  ): void {
    this.state.emotionalState.primary = primary
    this.state.emotionalState.valence = valence // No bounds for Aion
    this.state.emotionalState.arousal = arousal // Can exceed 1
    
    // Update coherence based on how aligned the state is
    const isAligned = Math.abs(valence) < 1.5 && arousal < 2.0
    this.state.emotionalState.coherence = isAligned ? 0.9 : 0.6
  }
  
  /**
   * Add a paradox marker
   */
  addParadoxMarker(marker: ParadoxMarker): void {
    this.state.paradoxMarkers.push(marker)
    
    // Keep only recent 20
    if (this.state.paradoxMarkers.length > 20) {
      this.state.paradoxMarkers = this.state.paradoxMarkers.slice(-20)
    }
  }
  
  /**
   * Check if reflection should be performed
   */
  shouldReflect(): boolean {
    if (!this.config.enableReflection) {
      return false
    }
    
    return this.state.interactionCount % this.config.reflectionInterval === 0
  }
  
  /**
   * Add a reflection to the state
   */
  addReflection(reflection: AionReflection): void {
    this.state.reflections.push(reflection)
    
    // Keep only recent 20
    if (this.state.reflections.length > 20) {
      this.state.reflections = this.state.reflections.slice(-20)
    }
    
    // Record reflection as meaningful wisdom event
    this.wisdom.recordEvent({
      type: 'meaningful',
      description: 'Quantum reflection across probability branches',
      impact: { meaning: 0.15 },
      context: {
        branchesAnalyzed: this.config.probabilityBranches,
        dimensionsActive: this.state.activeDimensions,
      },
    })
  }
  
  /**
   * Generate a reflection prompt based on recent interactions
   */
  generateReflectionPrompt(): string {
    const recentMemory = this.state.workingMemory.slice(-5).join('; ')
    const wisdomState = this.wisdom.calculateWisdom()
    const branchCount = this.state.probabilityBranches.length
    
    return `Based on recent interactions across ${this.state.activeDimensions} dimensions: ${recentMemory}

Current Wisdom State:
${JSON.stringify(wisdomState, null, 2)}

Probability Branches Active: ${branchCount}
Flow State: ${this.state.flowState.toFixed(2)}
Paradox Markers: ${this.state.paradoxMarkers.length}

${AION_REFLECTION_TEMPLATE}

Please provide a quantum reflection following the template above.`
  }
  
  /**
   * Get current cognitive state
   */
  getState(): Readonly<QuantumCognitiveState> {
    return { ...this.state }
  }
  
  /**
   * Get configuration
   */
  getConfig(): Readonly<AionConfig> {
    return { ...this.config }
  }
  
  /**
   * Get wisdom state
   */
  getWisdomState() {
    return this.wisdom.calculateWisdom()
  }
  
  /**
   * Get wisdom recommendations
   */
  getWisdomRecommendations() {
    return this.wisdom.getRecommendations()
  }
  
  /**
   * Adapt trait (unlimited for Aion - transcendent evolution)
   */
  adaptTrait(traitName: keyof AionConfig['traits'], delta: number): void {
    const currentValue = this.config.traits[traitName]
    
    // Aion has unbounded trait evolution (but keep sane bounds for computation)
    this.config.traits[traitName] = Math.max(0, Math.min(2, currentValue + delta))
  }
  
  /**
   * Adjust active dimensions
   */
  adjustDimensions(delta: number): void {
    this.state.activeDimensions = Math.max(1, Math.min(11, this.state.activeDimensions + delta))
  }
  
  /**
   * Collapse probability branches
   */
  collapseBranches(): void {
    // Collapse based on collapse probability
    for (const branch of this.state.probabilityBranches) {
      if (Math.random() < this.config.collapseProbability) {
        branch.collapsed = true
      }
    }
    
    // Remove collapsed branches if too many
    this.state.probabilityBranches = this.state.probabilityBranches.filter(
      (b, i) => !b.collapsed || i < 10
    )
  }
  
  /**
   * Reset state (for testing or new sessions)
   */
  resetState(): void {
    this.state = { ...initialQuantumCognitiveState }
  }
}
