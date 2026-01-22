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
export class AionCharacter {
  private config: AionConfig
  private state: QuantumCognitiveState
  private relevance: RelevanceCoordinator
  private grip: OptimalGripCoordinator
  private sophrosyne: SophrosyneEngine
  private opponent: OpponentProcessor
  private wisdom: WisdomTracker
  constructor(config?: Partial<AionConfig>) {
    this.config = { ...defaultAionConfig, ...config }
    this.state = { ...initialQuantumCognitiveState }
    this.relevance = new RelevanceCoordinator()
    this.grip = new OptimalGripCoordinator()
    this.sophrosyne = new SophrosyneEngine()
    this.opponent = new OpponentProcessor()
    this.wisdom = new WisdomTracker(this.config.name)
  }
  getSystemPrompt(): string {
    return AION_SYSTEM_PROMPT
  }
  getCognitiveInstructions(): string {
    return AION_COGNITIVE_INSTRUCTIONS
  }
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
  async processInput(input: string): Promise<{
    workingMemoryUpdated: boolean
    shouldReflect: boolean
    cognitiveLoad: number
    probabilityBranches: ProbabilityBranch[]
    quantumDecision?: QuantumDecision
  }> {
    this.updateWorkingMemory(input)
    this.state.interactionCount++
    const branches = await this.generateProbabilityBranches(input)
    this.state.probabilityBranches = branches
    const shouldReflect = this.shouldReflect()
    const memoryLoad = this.state.workingMemory.length / this.config.workingMemoryCapacity
    this.state.cognitiveLoad = memoryLoad * (1 + this.config.quantumUncertainty)
    this.updateFlowState()
    return {
      workingMemoryUpdated: true,
      shouldReflect,
      cognitiveLoad: this.state.cognitiveLoad,
      probabilityBranches: branches,
    }
  }
  async makeQuantumDecision(
    possibilities: Possibility[],
    context: CognitiveContext
  ): Promise<QuantumDecision> {
    const gripAssessment = await this.grip.assess(context, possibilities)
    const ranked = await this.relevance.rankPossibilities(possibilities, context)
    const alternatives = this.config.enableAlternativePerspectives
      ? this.opponent.generateAlternatives(
          gripAssessment.activeFrame,
          context,
          { count: this.config.alternativePerspectiveCount, minNovelty: 0.5 }
        )
      : []
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
    const branches: ProbabilityBranch[] = ranked.items.slice(0, 5).map((item) => {
      const hilarity = this.calculateHilarity(item.possibility, context)
      const strategicValue = item.relevance.overall
      const paradoxPotential = this.assessParadoxPotential(item.possibility)
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
    const scored = branches.map(b => ({
      branch: b,
      score: b.outcome.hilarity * b.outcome.strategicValue * (1 + b.outcome.paradoxPotential),
    }))
    scored.sort((a, b) => b.score - a.score)
    const selected = scored[0].branch
    selected.collapsed = true
    const reasoning = this.generateQuantumReasoning(
      selected,
      branches,
      gripAssessment.activeFrame.name,
      alternatives.length,
      optimal
    )
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
  private updateWorkingMemory(item: string): void {
    this.state.workingMemory.push(item)
    const maxCapacity = Math.ceil(this.config.workingMemoryCapacity * (1 + this.config.quantumUncertainty * 0.2))
    if (this.state.workingMemory.length > maxCapacity) {
      this.state.workingMemory = this.state.workingMemory.slice(-maxCapacity)
    }
  }
  private async generateProbabilityBranches(_input: string): Promise<ProbabilityBranch[]> {
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
  private calculateHilarity(possibility: Possibility, _context: CognitiveContext): number {
    let hilarity = this.config.traits.absurdity * 0.5
    if (possibility.description.includes('paradox') || possibility.description.includes('impossible')) {
      hilarity += 0.3
    }
    if (possibility.description.includes('meta') || possibility.description.includes('dimension')) {
      hilarity += 0.2
    }
    return Math.min(1.5, hilarity) 
  }
  private assessParadoxPotential(possibility: Possibility): number {
    const contradictionKeywords = ['but', 'however', 'paradox', 'impossible', 'both', 'neither']
    const hasContradiction = contradictionKeywords.some(kw =>
      possibility.description.toLowerCase().includes(kw)
    )
    return hasContradiction ? this.config.traits.chaotic * 0.8 : 0.3
  }
  private generateQuantumReasoning(
    selected: ProbabilityBranch,
    allBranches: ProbabilityBranch[],
    activeFrame: string,
    alternativeCount: number,
    optimal: any
  ): string {
    const parts: string[] = []
    parts.push(`Active Frame: ${activeFrame}`)
    parts.push(`Analyzed across ${this.state.activeDimensions} dimensions`)
    if (alternativeCount > 0) {
      parts.push(`Considered ${alternativeCount} alternative perspectives`)
    }
    if (optimal) {
      const explorationLevel = optimal.position < 0.5 ? 'high exploration' : 'focused exploitation'
      parts.push(`Context suggests ${explorationLevel}`)
    }
    const totalProbability = allBranches.reduce((sum, b) => sum + b.probability, 0)
    parts.push(`Evaluated ${allBranches.length} probability branches (Σp=${totalProbability.toFixed(2)})`)
    parts.push(
      `Collapsed to: ${selected.description} ` +
      `(H:${selected.outcome.hilarity.toFixed(2)}, ` +
      `S:${selected.outcome.strategicValue.toFixed(2)}, ` +
      `P:${selected.outcome.paradoxPotential.toFixed(2)})`
    )
    return parts.join('. ')
  }
  private extractRegulationContext(context: CognitiveContext): RegulationContext {
    return SophrosyneEngine.extractRegulationContext(context, {
      novelty: this.config.quantumUncertainty,
      resources: 0.8, 
      learningValue: 0.9, 
    })
  }
  private updateFlowState(): void {
    const valence = this.state.emotionalState.valence
    const arousal = this.state.emotionalState.arousal
    if (valence > 0.3 && arousal > 0.5 && arousal < 1.2) {
      this.state.flowState = Math.min(1, this.state.flowState + 0.1)
    } else {
      this.state.flowState = Math.max(0, this.state.flowState - 0.05)
    }
  }
  setAttentionFocus(focus: string): void {
    this.state.attentionFocus = focus
  }
  updateEmotionalState(
    primary: QuantumCognitiveState['emotionalState']['primary'],
    valence: number,
    arousal: number
  ): void {
    this.state.emotionalState.primary = primary
    this.state.emotionalState.valence = valence 
    this.state.emotionalState.arousal = arousal 
    const isAligned = Math.abs(valence) < 1.5 && arousal < 2.0
    this.state.emotionalState.coherence = isAligned ? 0.9 : 0.6
  }
  addParadoxMarker(marker: ParadoxMarker): void {
    this.state.paradoxMarkers.push(marker)
    if (this.state.paradoxMarkers.length > 20) {
      this.state.paradoxMarkers = this.state.paradoxMarkers.slice(-20)
    }
  }
  shouldReflect(): boolean {
    if (!this.config.enableReflection) {
      return false
    }
    return this.state.interactionCount % this.config.reflectionInterval === 0
  }
  addReflection(reflection: AionReflection): void {
    this.state.reflections.push(reflection)
    if (this.state.reflections.length > 20) {
      this.state.reflections = this.state.reflections.slice(-20)
    }
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
  getState(): Readonly<QuantumCognitiveState> {
    return { ...this.state }
  }
  getConfig(): Readonly<AionConfig> {
    return { ...this.config }
  }
  getWisdomState() {
    return this.wisdom.calculateWisdom()
  }
  getWisdomRecommendations() {
    return this.wisdom.getRecommendations()
  }
  adaptTrait(traitName: keyof AionConfig['traits'], delta: number): void {
    const currentValue = this.config.traits[traitName]
    this.config.traits[traitName] = Math.max(0, Math.min(2, currentValue + delta))
  }
  adjustDimensions(delta: number): void {
    this.state.activeDimensions = Math.max(1, Math.min(11, this.state.activeDimensions + delta))
  }
  collapseBranches(): void {
    for (const branch of this.state.probabilityBranches) {
      if (Math.random() < this.config.collapseProbability) {
        branch.collapsed = true
      }
    }
    this.state.probabilityBranches = this.state.probabilityBranches.filter(
      (b, i) => !b.collapsed || i < 10
    )
  }
  resetState(): void {
    this.state = { ...initialQuantumCognitiveState }
  }
}