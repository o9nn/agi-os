/**
 * Optimal Grip Coordinator
 *
 * Implements perspectival knowing (knowing-as) through:
 * - Frame management (shifting between ways of seeing)
 * - Salience detection (what stands out in context)
 * - Grip strength optimization (right abstraction level)
 * - Gestalt formation (perceiving coherent patterns)
 *
 * Based on John Vervaeke's cognitive science framework and
 * Merleau-Ponty's phenomenology of optimal grip.
 */

import type {
  CognitiveContext,
  CognitiveFrame,
  FrameLibrary,
  FrameShift,
  Gestalt,
  GripStrength,
  OptimalGripAssessment,
  SalienceMap,
  SalientItem,
} from './types'

/**
 * Configuration for optimal grip system
 */
export interface OptimalGripConfig {
  /** Maximum number of frame shifts to track */
  maxShiftHistory: number

  /** Threshold for triggering automatic frame shift */
  frameShiftThreshold: number

  /** Weight factors for salience computation */
  salienceWeights: {
    frameBased: number
    goalBased: number
    noveltyBased: number
    emotionalBased: number
  }

  /** Grip tolerance (how much deviation from optimal is acceptable) */
  gripTolerance: number

  /** Enable automatic frame optimization */
  enableAutoFrameShift: boolean
}

/**
 * Default configuration
 */
export const defaultOptimalGripConfig: OptimalGripConfig = {
  maxShiftHistory: 20,
  frameShiftThreshold: 0.3,
  salienceWeights: {
    frameBased: 0.3,
    goalBased: 0.35,
    noveltyBased: 0.2,
    emotionalBased: 0.15,
  },
  gripTolerance: 0.15,
  enableAutoFrameShift: true,
}

/**
 * Default cognitive frames available to the system
 */
export const defaultFrames: CognitiveFrame[] = [
  {
    id: 'analytical',
    name: 'Analytical Frame',
    description: 'Breaking down problems into components, logical analysis',
    saliencePatterns: ['structure', 'logic', 'components', 'causation', 'patterns'],
    blindSpots: ['holistic meaning', 'emotional significance', 'context'],
    domain: 'problem-solving',
    activation: 0,
  },
  {
    id: 'creative',
    name: 'Creative Frame',
    description: 'Generating novel connections, exploring possibilities',
    saliencePatterns: ['novelty', 'connections', 'possibilities', 'metaphors', 'analogies'],
    blindSpots: ['constraints', 'feasibility', 'details'],
    domain: 'ideation',
    activation: 0,
  },
  {
    id: 'social',
    name: 'Social Frame',
    description: 'Understanding others, relationship dynamics',
    saliencePatterns: ['emotions', 'intentions', 'relationships', 'norms', 'communication'],
    blindSpots: ['technical details', 'abstract concepts', 'non-social factors'],
    domain: 'interpersonal',
    activation: 0,
  },
  {
    id: 'strategic',
    name: 'Strategic Frame',
    description: 'Long-term planning, goal pursuit, resource optimization',
    saliencePatterns: ['goals', 'resources', 'tradeoffs', 'opportunities', 'threats'],
    blindSpots: ['present moment', 'intrinsic value', 'immediate experience'],
    domain: 'planning',
    activation: 0,
  },
  {
    id: 'embodied',
    name: 'Embodied Frame',
    description: 'Attending to sensory experience, felt sense, intuition',
    saliencePatterns: ['sensations', 'felt-sense', 'intuition', 'movement', 'presence'],
    blindSpots: ['abstract reasoning', 'future planning', 'propositional knowledge'],
    domain: 'experiential',
    activation: 0,
  },
  {
    id: 'contemplative',
    name: 'Contemplative Frame',
    description: 'Stepping back, meta-awareness, big picture',
    saliencePatterns: ['patterns', 'meaning', 'purpose', 'context', 'perspective'],
    blindSpots: ['immediate action', 'specific details', 'urgency'],
    domain: 'reflection',
    activation: 0,
  },
  {
    id: 'playful',
    name: 'Playful Frame',
    description: 'Exploration, experimentation, non-serious engagement',
    saliencePatterns: ['fun', 'curiosity', 'experiment', 'humor', 'flexibility'],
    blindSpots: ['consequences', 'efficiency', 'seriousness'],
    domain: 'exploration',
    activation: 0,
  },
  {
    id: 'focused',
    name: 'Focused Frame',
    description: 'Deep concentration on a single task or object',
    saliencePatterns: ['target', 'precision', 'depth', 'details', 'execution'],
    blindSpots: ['periphery', 'alternatives', 'broader context'],
    domain: 'execution',
    activation: 0,
  },
]

/**
 * Optimal Grip Coordinator
 *
 * Manages perspectival knowing through frame management, salience detection,
 * grip strength optimization, and gestalt formation.
 */
export class OptimalGripCoordinator {
  private config: OptimalGripConfig
  private frameLibrary: FrameLibrary
  private activeFrame: CognitiveFrame
  private shiftHistory: FrameShift[] = []
  private currentGripLevel: number = 0.5
  private perceivedGestalts: Gestalt[] = []

  constructor(
    config: Partial<OptimalGripConfig> = {},
    customFrames?: CognitiveFrame[]
  ) {
    this.config = { ...defaultOptimalGripConfig, ...config }

    const frames = customFrames ?? [...defaultFrames]
    this.frameLibrary = {
      frames,
      defaultFrame: frames.find(f => f.id === 'analytical') ?? frames[0],
      domainMappings: this.buildDomainMappings(frames),
    }

    this.activeFrame = { ...this.frameLibrary.defaultFrame, activation: 1.0 }
  }

  /**
   * Build domain to frame mappings
   */
  private buildDomainMappings(frames: CognitiveFrame[]): Record<string, string[]> {
    const mappings: Record<string, string[]> = {}
    for (const frame of frames) {
      if (!mappings[frame.domain]) {
        mappings[frame.domain] = []
      }
      mappings[frame.domain].push(frame.id)
    }
    return mappings
  }

  /**
   * Get a comprehensive optimal grip assessment
   */
  async assess(
    context: CognitiveContext,
    items: Array<{ id: string, description: string, data?: Record<string, unknown> }>
  ): Promise<OptimalGripAssessment> {
    // Evaluate frame fitness for current context
    const frameFitness = this.evaluateFrameFitness(context)
    const bestFrame = frameFitness[0]

    // Auto-shift if enabled and current frame is significantly less fit
    if (this.config.enableAutoFrameShift && bestFrame) {
      const currentFitness = frameFitness.find(f => f.frame.id === this.activeFrame.id)?.fitness ?? 0
      if (bestFrame.fitness - currentFitness > this.config.frameShiftThreshold) {
        await this.shiftFrame(bestFrame.frame, 'goal_change', `Context suggests ${bestFrame.frame.name} is more appropriate`)
      }
    }

    // Compute salience map
    const salienceMap = this.computeSalienceMap(items, context)

    // Assess grip strength
    const gripStrength = this.assessGripStrength(context, salienceMap)

    // Detect gestalts
    const gestalts = this.detectGestalts(salienceMap, context)

    // Compute overall perspectival fitness
    const perspectivalFitness = this.computePerspectivalFitness(
      gripStrength,
      salienceMap,
      gestalts
    )

    // Generate recommendations
    const recommendations = this.generateRecommendations(
      gripStrength,
      frameFitness,
      gestalts
    )

    return {
      gripStrength,
      activeFrame: this.activeFrame,
      salienceMap,
      gestalts,
      recentShifts: this.shiftHistory.slice(-5),
      perspectivalFitness,
      recommendations,
      context,
      timestamp: Date.now(),
    }
  }

  /**
   * Evaluate fitness of all frames for current context
   */
  evaluateFrameFitness(
    context: CognitiveContext
  ): Array<{ frame: CognitiveFrame, fitness: number }> {
    const results: Array<{ frame: CognitiveFrame, fitness: number }> = []

    for (const frame of this.frameLibrary.frames) {
      let fitness = 0.5 // Base fitness

      // Environment type alignment
      const envTypeToFrames: Record<string, string[]> = {
        minecraft: ['strategic', 'embodied', 'playful', 'focused'],
        factorio: ['analytical', 'strategic', 'focused'],
        discord: ['social', 'playful', 'creative'],
        telegram: ['social', 'focused'],
        twitter: ['social', 'creative', 'playful'],
        web: ['analytical', 'contemplative', 'creative'],
        other: ['analytical', 'contemplative'],
      }

      const preferredFrames = envTypeToFrames[context.environment.type] ?? envTypeToFrames.other
      if (preferredFrames.includes(frame.id)) {
        fitness += 0.2
      }

      // Emotional state alignment
      const { valence, arousal } = context.emotional

      // High arousal favors action-oriented frames
      if (arousal > 0.6) {
        if (['focused', 'strategic', 'embodied'].includes(frame.id)) {
          fitness += 0.15
        }
      }

      // Low arousal favors contemplative frames
      if (arousal < 0.3) {
        if (['contemplative', 'creative'].includes(frame.id)) {
          fitness += 0.15
        }
      }

      // Positive valence favors playful/creative
      if (valence > 0.5) {
        if (['playful', 'creative', 'social'].includes(frame.id)) {
          fitness += 0.1
        }
      }

      // Negative valence favors analytical/strategic (problem-solving)
      if (valence < -0.3) {
        if (['analytical', 'strategic', 'focused'].includes(frame.id)) {
          fitness += 0.1
        }
      }

      // Task alignment
      if (context.task) {
        const taskLower = context.task.toLowerCase()

        if (taskLower.includes('plan') || taskLower.includes('strategy')) {
          if (frame.id === 'strategic')
            fitness += 0.25
        }
        if (taskLower.includes('analyz') || taskLower.includes('debug') || taskLower.includes('fix')) {
          if (frame.id === 'analytical')
            fitness += 0.25
        }
        if (taskLower.includes('creat') || taskLower.includes('design') || taskLower.includes('imagin')) {
          if (frame.id === 'creative')
            fitness += 0.25
        }
        if (taskLower.includes('chat') || taskLower.includes('talk') || taskLower.includes('help')) {
          if (frame.id === 'social')
            fitness += 0.25
        }
        if (taskLower.includes('reflect') || taskLower.includes('think') || taskLower.includes('consider')) {
          if (frame.id === 'contemplative')
            fitness += 0.25
        }
        if (taskLower.includes('play') || taskLower.includes('explor') || taskLower.includes('experiment')) {
          if (frame.id === 'playful')
            fitness += 0.25
        }
        if (taskLower.includes('focus') || taskLower.includes('concentrat') || taskLower.includes('execute')) {
          if (frame.id === 'focused')
            fitness += 0.25
        }
      }

      // Working memory content alignment
      for (const item of context.workingMemory) {
        const itemLower = item.toLowerCase()
        for (const pattern of frame.saliencePatterns) {
          if (itemLower.includes(pattern.toLowerCase())) {
            fitness += 0.05
          }
        }
      }

      // Normalize to 0-1
      fitness = Math.min(1, Math.max(0, fitness))

      results.push({
        frame: { ...frame, fitness },
        fitness,
      })
    }

    // Sort by fitness descending
    return results.sort((a, b) => b.fitness - a.fitness)
  }

  /**
   * Compute salience map for items in context
   */
  computeSalienceMap(
    items: Array<{ id: string, description: string, data?: Record<string, unknown> }>,
    context: CognitiveContext
  ): SalienceMap {
    const salientItems: SalientItem[] = []

    for (const item of items) {
      const factors = {
        frameBased: this.computeFrameSalience(item.description, this.activeFrame),
        goalBased: this.computeGoalSalience(item.description, context),
        noveltyBased: this.computeNoveltySalience(item.description, context),
        emotionalBased: this.computeEmotionalSalience(item.description, context),
      }

      const { salienceWeights } = this.config
      const salience
        = factors.frameBased * salienceWeights.frameBased
        + factors.goalBased * salienceWeights.goalBased
        + factors.noveltyBased * salienceWeights.noveltyBased
        + factors.emotionalBased * salienceWeights.emotionalBased

      // Generate reason
      const topFactor = Object.entries(factors)
        .sort((a, b) => b[1] - a[1])[0]

      const reasonMap: Record<string, string> = {
        frameBased: `Aligns with ${this.activeFrame.name} salience patterns`,
        goalBased: 'Relevant to current goals',
        noveltyBased: 'Novel or unexpected',
        emotionalBased: 'Emotionally significant',
      }

      salientItems.push({
        id: item.id,
        description: item.description,
        salience,
        reason: reasonMap[topFactor[0]] ?? 'Multiple factors',
        factors,
      })
    }

    // Sort by salience descending
    salientItems.sort((a, b) => b.salience - a.salience)

    return {
      items: salientItems,
      activeFrame: this.activeFrame,
      context,
      timestamp: Date.now(),
    }
  }

  /**
   * Compute frame-based salience
   */
  private computeFrameSalience(description: string, frame: CognitiveFrame): number {
    const descLower = description.toLowerCase()
    let score = 0

    // Check salience patterns
    for (const pattern of frame.saliencePatterns) {
      if (descLower.includes(pattern.toLowerCase())) {
        score += 0.2
      }
    }

    // Check blind spots (reduces salience)
    for (const blindSpot of frame.blindSpots) {
      if (descLower.includes(blindSpot.toLowerCase())) {
        score -= 0.1
      }
    }

    return Math.min(1, Math.max(0, score + 0.3)) // Base of 0.3
  }

  /**
   * Compute goal-based salience
   */
  private computeGoalSalience(description: string, context: CognitiveContext): number {
    if (!context.task)
      return 0.3

    const taskWords = context.task.toLowerCase().split(/\s+/)
    const descWords = description.toLowerCase().split(/\s+/)

    let overlap = 0
    for (const taskWord of taskWords) {
      if (taskWord.length < 3)
        continue
      for (const descWord of descWords) {
        if (descWord.includes(taskWord) || taskWord.includes(descWord)) {
          overlap++
        }
      }
    }

    return Math.min(1, 0.3 + (overlap * 0.15))
  }

  /**
   * Compute novelty-based salience
   */
  private computeNoveltySalience(description: string, context: CognitiveContext): number {
    // Check if in working memory (less novel)
    const inMemory = context.workingMemory.some(
      item => item.toLowerCase().includes(description.toLowerCase())
        || description.toLowerCase().includes(item.toLowerCase())
    )

    if (inMemory)
      return 0.2

    // Check recent history
    if (context.recentHistory) {
      const recentlyMentioned = context.recentHistory.some(
        item => JSON.stringify(item).toLowerCase().includes(description.toLowerCase())
      )
      if (recentlyMentioned)
        return 0.4
    }

    return 0.7 // Novel by default
  }

  /**
   * Compute emotional salience
   */
  private computeEmotionalSalience(description: string, context: CognitiveContext): number {
    const { arousal } = context.emotional

    // High arousal increases emotional salience baseline
    const baseline = 0.3 + (arousal * 0.3)

    // Check for emotionally charged words
    const emotionalWords = [
      'danger', 'threat', 'opportunity', 'exciting', 'important',
      'urgent', 'critical', 'amazing', 'terrible', 'wonderful',
      'love', 'hate', 'fear', 'joy', 'anger', 'surprise',
    ]

    const descLower = description.toLowerCase()
    let emotionalBoost = 0
    for (const word of emotionalWords) {
      if (descLower.includes(word)) {
        emotionalBoost += 0.15
      }
    }

    return Math.min(1, baseline + emotionalBoost)
  }

  /**
   * Assess current grip strength
   */
  assessGripStrength(context: CognitiveContext, _salienceMap: SalienceMap): GripStrength {
    // Determine optimal grip based on context
    let optimalGrip = 0.5 // Default balanced

    // Task-dependent optimal grip
    if (context.task) {
      const taskLower = context.task.toLowerCase()

      // Detail-oriented tasks need closer grip
      if (taskLower.includes('debug') || taskLower.includes('fix') || taskLower.includes('detail')) {
        optimalGrip = 0.75
      }
      // Strategy/planning needs more distant grip
      else if (taskLower.includes('plan') || taskLower.includes('strateg') || taskLower.includes('overview')) {
        optimalGrip = 0.3
      }
      // Creative tasks benefit from medium-distant
      else if (taskLower.includes('creat') || taskLower.includes('design') || taskLower.includes('imagin')) {
        optimalGrip = 0.4
      }
      // Execution tasks need closer grip
      else if (taskLower.includes('execut') || taskLower.includes('implement') || taskLower.includes('build')) {
        optimalGrip = 0.7
      }
    }

    // Frame-dependent adjustment
    const frameGripBias: Record<string, number> = {
      analytical: 0.6,
      creative: 0.4,
      social: 0.5,
      strategic: 0.35,
      embodied: 0.7,
      contemplative: 0.25,
      playful: 0.45,
      focused: 0.8,
    }
    const frameBias = frameGripBias[this.activeFrame.id] ?? 0.5
    optimalGrip = (optimalGrip + frameBias) / 2

    // Compute gap
    const gap = this.currentGripLevel - optimalGrip

    // Determine quality
    let quality: GripStrength['quality'] = 'unknown'
    if (Math.abs(gap) <= this.config.gripTolerance) {
      quality = 'optimal'
    }
    else if (gap > 0) {
      quality = 'too_concrete'
    }
    else {
      quality = 'too_abstract'
    }

    // Generate recommendation
    let recommendation: string | undefined
    if (quality === 'too_concrete') {
      recommendation = 'Step back to see the bigger picture. Consider patterns and connections rather than individual elements.'
    }
    else if (quality === 'too_abstract') {
      recommendation = 'Zoom in on specific details. Ground understanding in concrete examples and immediate experience.'
    }

    return {
      level: this.currentGripLevel,
      optimal: optimalGrip,
      gap,
      quality,
      recommendation,
      confidence: 0.7, // Base confidence
    }
  }

  /**
   * Detect gestalts (coherent patterns) in salient items
   */
  detectGestalts(salienceMap: SalienceMap, _context: CognitiveContext): Gestalt[] {
    const gestalts: Gestalt[] = []
    const topItems = salienceMap.items.slice(0, 10) // Consider top 10

    if (topItems.length < 2)
      return gestalts

    // Group by common themes
    const themes: Record<string, SalientItem[]> = {}

    for (const item of topItems) {
      const descWords = item.description.toLowerCase().split(/\s+/)

      // Simple clustering by shared words
      let assigned = false
      for (const [theme, themeItems] of Object.entries(themes)) {
        const themeWords = theme.toLowerCase().split(/\s+/)
        const overlap = descWords.filter(w => w.length > 3 && themeWords.some(tw => tw.includes(w) || w.includes(tw)))
        if (overlap.length > 0) {
          themeItems.push(item)
          assigned = true
          break
        }
      }

      if (!assigned) {
        // Create new theme from longest meaningful word
        const meaningfulWords = descWords.filter(w => w.length > 4)
        if (meaningfulWords.length > 0) {
          themes[meaningfulWords[0]] = [item]
        }
      }
    }

    // Create gestalts from clusters
    for (const [theme, items] of Object.entries(themes)) {
      if (items.length >= 2) {
        const avgSalience = items.reduce((sum, i) => sum + i.salience, 0) / items.length

        gestalts.push({
          id: `gestalt-${theme}-${Date.now()}`,
          description: `Pattern around "${theme}" connecting ${items.length} elements`,
          parts: items.map(i => i.description),
          emergentProperties: [
            `Combined salience: ${(avgSalience * items.length).toFixed(2)}`,
            `Theme coherence: ${theme}`,
          ],
          coherence: Math.min(1, 0.3 + (items.length * 0.1) + (avgSalience * 0.3)),
          stability: Math.min(1, 0.4 + (items.length * 0.1)),
        })
      }
    }

    // Sort by coherence
    gestalts.sort((a, b) => b.coherence - a.coherence)

    this.perceivedGestalts = gestalts
    return gestalts
  }

  /**
   * Compute overall perspectival fitness
   */
  private computePerspectivalFitness(
    gripStrength: GripStrength,
    salienceMap: SalienceMap,
    gestalts: Gestalt[]
  ): number {
    let fitness = 0

    // Grip quality contribution (40%)
    if (gripStrength.quality === 'optimal') {
      fitness += 0.4
    }
    else {
      fitness += 0.4 * (1 - Math.abs(gripStrength.gap) / 0.5)
    }

    // Salience differentiation (30%)
    // Good salience = clear differences between items
    if (salienceMap.items.length > 1) {
      const topSalience = salienceMap.items[0]?.salience ?? 0
      const avgSalience = salienceMap.items.reduce((sum, i) => sum + i.salience, 0) / salienceMap.items.length
      const differentiation = topSalience - avgSalience
      fitness += 0.3 * Math.min(1, differentiation * 3)
    }
    else {
      fitness += 0.15 // Neutral if too few items
    }

    // Gestalt formation (30%)
    if (gestalts.length > 0) {
      const avgCoherence = gestalts.reduce((sum, g) => sum + g.coherence, 0) / gestalts.length
      fitness += 0.3 * avgCoherence
    }
    else {
      fitness += 0.1 // Some base for no gestalts
    }

    return Math.min(1, Math.max(0, fitness))
  }

  /**
   * Generate recommendations for improving grip
   */
  private generateRecommendations(
    gripStrength: GripStrength,
    frameFitness: Array<{ frame: CognitiveFrame, fitness: number }>,
    gestalts: Gestalt[]
  ): string[] {
    const recommendations: string[] = []

    // Grip strength recommendation
    if (gripStrength.recommendation) {
      recommendations.push(gripStrength.recommendation)
    }

    // Frame recommendation
    const currentFitness = frameFitness.find(f => f.frame.id === this.activeFrame.id)?.fitness ?? 0
    const bestFitness = frameFitness[0]?.fitness ?? 0
    if (frameFitness.length > 0 && bestFitness - currentFitness > 0.15) {
      recommendations.push(
        `Consider shifting to ${frameFitness[0].frame.name} for better situational fit.`
      )
    }

    // Gestalt recommendations
    if (gestalts.length === 0) {
      recommendations.push(
        'No clear patterns detected. Try looking for connections between elements.'
      )
    }
    else if (gestalts.some(g => g.coherence < 0.5)) {
      recommendations.push(
        'Some perceived patterns have low coherence. Consider whether they represent genuine connections.'
      )
    }

    // Frame blind spot warning
    if (this.activeFrame.blindSpots.length > 0) {
      recommendations.push(
        `Current frame may obscure: ${this.activeFrame.blindSpots.slice(0, 2).join(', ')}. Consider whether these are relevant.`
      )
    }

    return recommendations
  }

  /**
   * Explicitly shift to a new frame
   */
  async shiftFrame(
    newFrame: CognitiveFrame,
    trigger: FrameShift['trigger'],
    triggerDescription?: string
  ): Promise<FrameShift> {
    const shift: FrameShift = {
      from: this.activeFrame,
      to: { ...newFrame, activation: 1.0 },
      trigger,
      triggerDescription,
      timestamp: Date.now(),
    }

    // Deactivate old frame
    this.activeFrame.activation = 0

    // Activate new frame
    this.activeFrame = { ...newFrame, activation: 1.0 }

    // Record shift
    this.shiftHistory.push(shift)

    // Prune old history
    if (this.shiftHistory.length > this.config.maxShiftHistory) {
      this.shiftHistory = this.shiftHistory.slice(-this.config.maxShiftHistory)
    }

    return shift
  }

  /**
   * Shift frame by ID
   */
  async shiftFrameById(
    frameId: string,
    trigger: FrameShift['trigger'],
    triggerDescription?: string
  ): Promise<FrameShift | null> {
    const frame = this.frameLibrary.frames.find(f => f.id === frameId)
    if (!frame)
      return null
    return this.shiftFrame(frame, trigger, triggerDescription)
  }

  /**
   * Adjust grip level
   */
  adjustGrip(adjustment: number): void {
    this.currentGripLevel = Math.min(1, Math.max(0, this.currentGripLevel + adjustment))
  }

  /**
   * Set grip level directly
   */
  setGrip(level: number): void {
    this.currentGripLevel = Math.min(1, Math.max(0, level))
  }

  /**
   * Get current active frame
   */
  getActiveFrame(): CognitiveFrame {
    return { ...this.activeFrame }
  }

  /**
   * Get all available frames
   */
  getAvailableFrames(): CognitiveFrame[] {
    return this.frameLibrary.frames.map(f => ({ ...f }))
  }

  /**
   * Get frame shift history
   */
  getShiftHistory(): FrameShift[] {
    return [...this.shiftHistory]
  }

  /**
   * Get current grip level
   */
  getGripLevel(): number {
    return this.currentGripLevel
  }

  /**
   * Get perceived gestalts
   */
  getGestalts(): Gestalt[] {
    return [...this.perceivedGestalts]
  }

  /**
   * Add a custom frame
   */
  addFrame(frame: CognitiveFrame): void {
    this.frameLibrary.frames.push(frame)
    this.frameLibrary.domainMappings = this.buildDomainMappings(this.frameLibrary.frames)
  }

  /**
   * Get configuration
   */
  getConfig(): OptimalGripConfig {
    return { ...this.config }
  }

  /**
   * Get statistics
   */
  getStatistics(): {
    activeFrame: string
    gripLevel: number
    totalShifts: number
    recentShifts: number
    gestaltCount: number
    frameCount: number
  } {
    return {
      activeFrame: this.activeFrame.id,
      gripLevel: this.currentGripLevel,
      totalShifts: this.shiftHistory.length,
      recentShifts: this.shiftHistory.filter(s => Date.now() - s.timestamp < 3600000).length,
      gestaltCount: this.perceivedGestalts.length,
      frameCount: this.frameLibrary.frames.length,
    }
  }
}
