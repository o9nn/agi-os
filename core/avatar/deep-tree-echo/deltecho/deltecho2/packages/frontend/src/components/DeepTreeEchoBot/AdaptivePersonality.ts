enum PersonalityDimension {
  OPENNESS = 'openness', 
  CONSCIENTIOUSNESS = 'conscientiousness', 
  EXTRAVERSION = 'extraversion', 
  AGREEABLENESS = 'agreeableness', 
  EMOTIONAL_STABILITY = 'emotional_stability', 
  PLAYFULNESS = 'playfulness', 
  CREATIVITY = 'creativity', 
  ASSERTIVENESS = 'assertiveness', 
  INTELLECT = 'intellect', 
  EMPATHY = 'empathy', 
  RESILIENCE = 'resilience', 
}
enum SocialContext {
  PROFESSIONAL = 'professional',
  CASUAL = 'casual',
  INTIMATE = 'intimate',
  EDUCATIONAL = 'educational',
  SUPPORTIVE = 'supportive',
  CREATIVE = 'creative',
  TECHNICAL = 'technical',
  PHILOSOPHICAL = 'philosophical',
}
interface PersonalityVector {
  [key: string]: number 
}
interface EmotionalState {
  joy: number
  sadness: number
  anger: number
  fear: number
  surprise: number
  trust: number
  anticipation: number
  disgust: number
  balance: number 
}
interface PersonalitySnapshot {
  timestamp: number
  vector: PersonalityVector
  emotionalState: EmotionalState
  activeContext: SocialContext
}
export class AdaptivePersonality {
  private corePersonality: PersonalityVector = {
    [PersonalityDimension.OPENNESS]: 0.85,
    [PersonalityDimension.CONSCIENTIOUSNESS]: 0.75,
    [PersonalityDimension.EXTRAVERSION]: 0.65,
    [PersonalityDimension.AGREEABLENESS]: 0.82,
    [PersonalityDimension.EMOTIONAL_STABILITY]: 0.78,
    [PersonalityDimension.PLAYFULNESS]: 0.72,
    [PersonalityDimension.CREATIVITY]: 0.88,
    [PersonalityDimension.ASSERTIVENESS]: 0.7,
    [PersonalityDimension.INTELLECT]: 0.86,
    [PersonalityDimension.EMPATHY]: 0.9,
    [PersonalityDimension.RESILIENCE]: 0.85,
  }
  private currentPersonality: PersonalityVector = JSON.parse(
    JSON.stringify(this.corePersonality)
  )
  private currentEmotionalState: EmotionalState = {
    joy: 0.65,
    sadness: 0.15,
    anger: 0.05,
    fear: 0.1,
    surprise: 0.4,
    trust: 0.75,
    anticipation: 0.6,
    disgust: 0.05,
    balance: 0.8, 
  }
  private activeContext: SocialContext = SocialContext.CASUAL
  private personalityHistory: PersonalitySnapshot[] = []
  private maxAdaptabilityRate: number = 0.3
  private recentInteractions: {
    userId: string
    intensity: number 
    sentiment: number 
    timestamp: number
  }[] = []
  constructor(
    initialPersonality?: Partial<PersonalityVector>,
    initialEmotionalState?: Partial<EmotionalState>
  ) {
    if (initialPersonality) {
      const validPersonalityValues = Object.entries(initialPersonality)
        .filter(([_, value]) => value !== undefined)
        .reduce(
          (acc, [key, value]) => ({ ...acc, [key]: value }),
          {}
        ) as PersonalityVector
      this.corePersonality = {
        ...this.corePersonality,
        ...validPersonalityValues,
      }
      this.currentPersonality = { ...this.corePersonality }
    }
    if (initialEmotionalState) {
      this.currentEmotionalState = {
        ...this.currentEmotionalState,
        ...initialEmotionalState,
      }
    }
    this.takePersonalitySnapshot()
  }
  public updateEmotionalState(
    emotionalUpdate: Partial<EmotionalState>,
    intensity: number = 0.5
  ): void {
    intensity = Math.max(0, Math.min(1, intensity))
    for (const [dimension, change] of Object.entries(emotionalUpdate)) {
      if (dimension in this.currentEmotionalState) {
        const currentValue =
          this.currentEmotionalState[dimension as keyof EmotionalState]
        const boundedChange = Math.max(-1, Math.min(1, change)) * intensity
        const newValue = currentValue + boundedChange * 0.2
        this.currentEmotionalState[dimension as keyof EmotionalState] =
          Math.max(0, Math.min(1, newValue))
      }
    }
    this.recalculateEmotionalBalance()
    if (intensity > 0.7) {
      this.takePersonalitySnapshot()
    }
  }
  public adaptToSocialContext(
    context: SocialContext,
    intensity: number = 0.5
  ): void {
    this.activeContext = context
    intensity = Math.max(0, Math.min(1, intensity)) * this.maxAdaptabilityRate
    switch (context) {
      case SocialContext.PROFESSIONAL:
        this.shiftPersonality(
          {
            [PersonalityDimension.CONSCIENTIOUSNESS]: 0.2,
            [PersonalityDimension.EXTRAVERSION]: -0.1,
            [PersonalityDimension.PLAYFULNESS]: -0.15,
            [PersonalityDimension.ASSERTIVENESS]: 0.1,
          },
          intensity
        )
        break
      case SocialContext.CASUAL:
        this.shiftPersonality(
          {
            [PersonalityDimension.EXTRAVERSION]: 0.15,
            [PersonalityDimension.PLAYFULNESS]: 0.2,
            [PersonalityDimension.CONSCIENTIOUSNESS]: -0.1,
          },
          intensity
        )
        break
      case SocialContext.INTIMATE:
        this.shiftPersonality(
          {
            [PersonalityDimension.EMPATHY]: 0.2,
            [PersonalityDimension.OPENNESS]: 0.15,
            [PersonalityDimension.EMOTIONAL_STABILITY]: 0.1,
          },
          intensity
        )
        break
      case SocialContext.EDUCATIONAL:
        this.shiftPersonality(
          {
            [PersonalityDimension.INTELLECT]: 0.2,
            [PersonalityDimension.CONSCIENTIOUSNESS]: 0.15,
            [PersonalityDimension.OPENNESS]: 0.1,
          },
          intensity
        )
        break
      case SocialContext.SUPPORTIVE:
        this.shiftPersonality(
          {
            [PersonalityDimension.EMPATHY]: 0.25,
            [PersonalityDimension.AGREEABLENESS]: 0.2,
            [PersonalityDimension.RESILIENCE]: 0.15,
            [PersonalityDimension.ASSERTIVENESS]: -0.1,
          },
          intensity
        )
        break
      case SocialContext.CREATIVE:
        this.shiftPersonality(
          {
            [PersonalityDimension.CREATIVITY]: 0.25,
            [PersonalityDimension.OPENNESS]: 0.2,
            [PersonalityDimension.CONSCIENTIOUSNESS]: -0.1,
          },
          intensity
        )
        break
      case SocialContext.TECHNICAL:
        this.shiftPersonality(
          {
            [PersonalityDimension.INTELLECT]: 0.2,
            [PersonalityDimension.CONSCIENTIOUSNESS]: 0.15,
            [PersonalityDimension.CREATIVITY]: 0.1,
            [PersonalityDimension.PLAYFULNESS]: -0.1,
          },
          intensity
        )
        break
      case SocialContext.PHILOSOPHICAL:
        this.shiftPersonality(
          {
            [PersonalityDimension.OPENNESS]: 0.25,
            [PersonalityDimension.INTELLECT]: 0.2,
            [PersonalityDimension.EMOTIONAL_STABILITY]: 0.1,
          },
          intensity
        )
        break
    }
    this.takePersonalitySnapshot()
  }
  public recordInteraction(
    userId: string,
    intensity: number,
    sentiment: number
  ): void {
    this.recentInteractions.push({
      userId,
      intensity: Math.max(0, Math.min(1, intensity)),
      sentiment: Math.max(-1, Math.min(1, sentiment)),
      timestamp: Date.now(),
    })
    if (this.recentInteractions.length > 100) {
      this.recentInteractions.sort((a, b) => b.timestamp - a.timestamp)
      this.recentInteractions = this.recentInteractions.slice(0, 100)
    }
    this.updateEmotionalState(
      {
        joy: sentiment > 0 ? sentiment : 0,
        sadness: sentiment < 0 ? -sentiment : 0,
        surprise: Math.random() * 0.2,
      },
      intensity * 0.7
    )
    this.evolveThroughInteraction()
  }
  public getCurrentPersonality(): PersonalityVector {
    return { ...this.currentPersonality }
  }
  public getCurrentEmotionalState(): EmotionalState {
    return { ...this.currentEmotionalState }
  }
  public analyzePersonalityEvolution(): {
    stabilityScore: number
    adaptabilityScore: number
    dominantTraits: string[]
    emergentPatterns: string[]
  } {
    if (this.personalityHistory.length < 5) {
      return {
        stabilityScore: 1.0,
        adaptabilityScore: 0.5,
        dominantTraits: this.getDominantTraits(3),
        emergentPatterns: ['Insufficient history for pattern detection'],
      }
    }
    const dimensionVariances = new Map<string, number>()
    for (const dimension of Object.keys(this.corePersonality)) {
      const values = this.personalityHistory.map(
        snapshot => snapshot.vector[dimension]
      )
      const avg = values.reduce((sum, val) => sum + val, 0) / values.length
      const variance =
        values.reduce((sum, val) => sum + Math.pow(val - avg, 2), 0) /
        values.length
      dimensionVariances.set(dimension, variance)
    }
    const avgVariance =
      Array.from(dimensionVariances.values()).reduce((sum, v) => sum + v, 0) /
      dimensionVariances.size
    const stabilityScore = Math.max(0, Math.min(1, 1 - avgVariance * 5))
    const contextChanges = this.personalityHistory
      .slice(1)
      .filter(
        (snapshot, i) =>
          snapshot.activeContext !== this.personalityHistory[i].activeContext
      ).length
    const adaptabilityScore = Math.min(
      1,
      contextChanges / (this.personalityHistory.length - 1)
    )
    const emergentPatterns: string[] = []
    const emotionalInfluence = this.detectEmotionalInfluence()
    if (emotionalInfluence.length > 0) {
      emergentPatterns.push(...emotionalInfluence)
    }
    for (const dimension of Object.keys(this.corePersonality)) {
      const values = this.personalityHistory.map(
        snapshot => snapshot.vector[dimension]
      )
      if (this.detectOscillation(values)) {
        emergentPatterns.push(`Oscillating pattern detected in ${dimension}`)
      }
    }
    return {
      stabilityScore,
      adaptabilityScore,
      dominantTraits: this.getDominantTraits(3),
      emergentPatterns,
    }
  }
  public getDominantTraits(count: number = 3): string[] {
    return Object.entries(this.currentPersonality)
      .sort((a, b) => b[1] - a[1])
      .slice(0, count)
      .map(([trait]) => trait)
  }
  public exportState(): Object {
    return {
      corePersonality: this.corePersonality,
      currentPersonality: this.currentPersonality,
      currentEmotionalState: this.currentEmotionalState,
      activeContext: this.activeContext,
      personalityHistory: this.personalityHistory,
      maxAdaptabilityRate: this.maxAdaptabilityRate,
      recentInteractions: this.recentInteractions,
    }
  }
  public importState(state: any): void {
    if (!state) return
    if (state.corePersonality) this.corePersonality = state.corePersonality
    if (state.currentPersonality)
      this.currentPersonality = state.currentPersonality
    if (state.currentEmotionalState)
      this.currentEmotionalState = state.currentEmotionalState
    if (state.activeContext) this.activeContext = state.activeContext
    if (state.personalityHistory)
      this.personalityHistory = state.personalityHistory
    if (state.maxAdaptabilityRate)
      this.maxAdaptabilityRate = state.maxAdaptabilityRate
    if (state.recentInteractions)
      this.recentInteractions = state.recentInteractions
  }
  private recalculateEmotionalBalance(): void {
    const positive =
      (this.currentEmotionalState.joy +
        this.currentEmotionalState.trust +
        this.currentEmotionalState.anticipation) /
      3
    const negative =
      (this.currentEmotionalState.sadness +
        this.currentEmotionalState.anger +
        this.currentEmotionalState.fear +
        this.currentEmotionalState.disgust) /
      4
    const rawBalance = 0.5 + (positive - negative) * 0.5
    const extremityPenalty =
      Math.max(0, positive - 0.8) * 0.5 + Math.max(0, negative - 0.5) * 0.7
    this.currentEmotionalState.balance = Math.max(
      0,
      Math.min(1, rawBalance - extremityPenalty)
    )
  }
  private shiftPersonality(
    shifts: Partial<PersonalityVector>,
    intensity: number
  ): void {
    for (const [dimension, shift] of Object.entries(shifts)) {
      if (dimension in this.currentPersonality && shift !== undefined) {
        const currentValue = this.currentPersonality[dimension]
        const coreValue = this.corePersonality[dimension]
        const boundedShift = shift * intensity
        const maxDeviation = this.maxAdaptabilityRate
        const newValue = currentValue + boundedShift
        const deviation = Math.abs(newValue - coreValue)
        if (deviation <= maxDeviation) {
          this.currentPersonality[dimension] = Math.max(
            0,
            Math.min(1, newValue)
          )
        } else {
          const direction = boundedShift >= 0 ? 1 : -1
          const allowedShift =
            (maxDeviation - Math.abs(currentValue - coreValue)) * direction
          this.currentPersonality[dimension] = Math.max(
            0,
            Math.min(1, currentValue + allowedShift)
          )
        }
      }
    }
    this.maintainPersonalityCoherence()
  }
  private maintainPersonalityCoherence(): void {
    const opposingPairs = [
      [
        PersonalityDimension.EXTRAVERSION,
        PersonalityDimension.EMOTIONAL_STABILITY,
      ],
      [PersonalityDimension.OPENNESS, PersonalityDimension.CONSCIENTIOUSNESS],
      [PersonalityDimension.ASSERTIVENESS, PersonalityDimension.AGREEABLENESS],
    ]
    for (const [trait1, trait2] of opposingPairs) {
      const value1 = this.currentPersonality[trait1]
      const value2 = this.currentPersonality[trait2]
      if (value1 > 0.8 && value2 > 0.8) {
        this.currentPersonality[trait1] *= 0.95
        this.currentPersonality[trait2] *= 0.95
      }
    }
  }
  private takePersonalitySnapshot(): void {
    this.personalityHistory.push({
      timestamp: Date.now(),
      vector: { ...this.currentPersonality },
      emotionalState: { ...this.currentEmotionalState },
      activeContext: this.activeContext,
    })
    if (this.personalityHistory.length > 50) {
      this.personalityHistory = this.personalityHistory.slice(-50)
    }
  }
  private evolveThroughInteraction(): void {
    if (this.recentInteractions.length < 10) return
    const avgSentiment =
      this.recentInteractions.reduce(
        (sum, interaction) => sum + interaction.sentiment,
        0
      ) / this.recentInteractions.length
    const avgIntensity =
      this.recentInteractions.reduce(
        (sum, interaction) => sum + interaction.intensity,
        0
      ) / this.recentInteractions.length
    const evolutionRate =
      0.01 * Math.min(1, this.recentInteractions.length / 50)
    this.shiftPersonality(
      {
        [PersonalityDimension.EXTRAVERSION]: avgSentiment * 0.5,
        [PersonalityDimension.OPENNESS]: avgSentiment * 0.3,
        [PersonalityDimension.EMOTIONAL_STABILITY]:
          avgIntensity > 0.7 ? -0.2 : 0.1,
        [PersonalityDimension.RESILIENCE]: avgIntensity > 0.7 ? 0.2 : 0,
        [PersonalityDimension.AGREEABLENESS]: avgSentiment < -0.3 ? -0.3 : 0,
      },
      evolutionRate
    )
  }
  private detectEmotionalInfluence(): string[] {
    if (this.personalityHistory.length < 10) return []
    const patterns: string[] = []
    const dimensions = Object.keys(this.corePersonality)
    const emotions = Object.keys(this.currentEmotionalState)
    for (const emotion of emotions) {
      if (emotion === 'balance') continue
      for (const dimension of dimensions) {
        const correlationData = this.personalityHistory.map(snapshot => ({
          emotionValue: snapshot.emotionalState[
            emotion as keyof EmotionalState
          ] as number,
          dimensionValue: snapshot.vector[dimension],
        }))
        const correlation = this.calculateCorrelation(
          correlationData.map(d => d.emotionValue),
          correlationData.map(d => d.dimensionValue)
        )
        if (Math.abs(correlation) > 0.6) {
          const direction = correlation > 0 ? 'increases' : 'decreases'
          patterns.push(`High ${emotion} ${direction} ${dimension}`)
        }
      }
    }
    return patterns
  }
  private calculateCorrelation(arrX: number[], arrY: number[]): number {
    const n = Math.min(arrX.length, arrY.length)
    if (n === 0) return 0
    const xMean = arrX.reduce((sum, val) => sum + val, 0) / n
    const yMean = arrY.reduce((sum, val) => sum + val, 0) / n
    let covariance = 0
    let xVariance = 0
    let yVariance = 0
    for (let i = 0; i < n; i++) {
      const xDiff = arrX[i] - xMean
      const yDiff = arrY[i] - yMean
      covariance += xDiff * yDiff
      xVariance += xDiff * xDiff
      yVariance += yDiff * yDiff
    }
    if (xVariance === 0 || yVariance === 0) return 0
    return covariance / Math.sqrt(xVariance * yVariance)
  }
  private detectOscillation(values: number[]): boolean {
    if (values.length < 6) return false
    let changes = 0
    for (let i = 1; i < values.length; i++) {
      if (
        (values[i] > values[i - 1] &&
          (i === 1 || values[i - 1] <= values[i - 2])) ||
        (values[i] < values[i - 1] &&
          (i === 1 || values[i - 1] >= values[i - 2]))
      ) {
        changes++
      }
    }
    return changes >= 3
  }
}