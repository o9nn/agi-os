enum EmotionDimension {
  JOY = 'joy',
  SADNESS = 'sadness',
  ANGER = 'anger',
  FEAR = 'fear',
  DISGUST = 'disgust',
  SURPRISE = 'surprise',
  TRUST = 'trust',
  ANTICIPATION = 'anticipation',
}
enum SecondaryEmotion {
  CONTENTMENT = 'contentment',
  HAPPINESS = 'happiness',
  AMUSEMENT = 'amusement',
  PRIDE = 'pride',
  OPTIMISM = 'optimism',
  ENTHUSIASM = 'enthusiasm',
  DISAPPOINTMENT = 'disappointment',
  GRIEF = 'grief',
  LONELINESS = 'loneliness',
  REGRET = 'regret',
  FRUSTRATION = 'frustration',
  IRRITATION = 'irritation',
  OUTRAGE = 'outrage',
  ANXIETY = 'anxiety',
  WORRY = 'worry',
  NERVOUSNESS = 'nervousness',
  ACCEPTANCE = 'acceptance',
  ADMIRATION = 'admiration',
  GRATITUDE = 'gratitude',
  COMPASSION = 'compassion', 
  JEALOUSY = 'jealousy', 
  CURIOSITY = 'curiosity', 
  NOSTALGIA = 'nostalgia', 
}
enum ConfidenceLevel {
  VERY_LOW = 'very_low', 
  LOW = 'low', 
  MODERATE = 'moderate', 
  HIGH = 'high', 
  VERY_HIGH = 'very_high', 
}
interface EmotionProfile {
  primaryEmotions: { [key in EmotionDimension]?: number }
  secondaryEmotions: { [key in SecondaryEmotion]?: number }
  valence: number
  arousal: number
  dominantEmotion?: EmotionDimension
  dominantSecondaryEmotion?: SecondaryEmotion
  confidence: ConfidenceLevel
  contextFactors: string[]
}
interface EmotionHistory {
  timestamp: number
  profile: EmotionProfile
  trigger?: string 
}
export class EmotionalIntelligence {
  private emotionHistory: EmotionHistory[] = []
  private currentEmotionProfile: EmotionProfile = this.createNeutralProfile()
  private sensitivityLevel: number = 0.7
  private emotionDecayRate: number = 0.9
  private patternDictionary: Map<string, number[]> = new Map()
  private empathyLevel: number = 0.85
  constructor() {
    this.resetEmotionalState()
    this.initializePatternDictionary()
  }
  public analyzeEmotion(
    text: string,
    intensity: number = 0.7,
    context: string[] = []
  ): EmotionProfile {
    const extractedEmotions = this.extractEmotionsFromText(text)
    const scaledEmotions = this.scaleEmotionIntensity(
      extractedEmotions,
      intensity
    )
    this.updateEmotionalState(scaledEmotions, context, text)
    return this.getCurrentEmotionProfile()
  }
  public getCurrentEmotionProfile(): EmotionProfile {
    return { ...this.currentEmotionProfile }
  }
  public resetEmotionalState(): void {
    this.currentEmotionProfile = this.createNeutralProfile()
    this.emotionHistory.push({
      timestamp: Date.now(),
      profile: { ...this.currentEmotionProfile },
      trigger: 'system_reset',
    })
  }
  public analyzeEmotionalTrends(): {
    dominantEmotions: [EmotionDimension, number][]
    volatility: number
    emotionalRange: number
    patterns: string[]
  } {
    if (this.emotionHistory.length < 3) {
      return {
        dominantEmotions: [],
        volatility: 0,
        emotionalRange: 0,
        patterns: [],
      }
    }
    const emotionSequences = this.extractEmotionSequences()
    const dominantEmotions = this.calculateDominantEmotions()
    const volatility = this.calculateEmotionalVolatility()
    const emotionalRange = this.calculateEmotionalRange()
    const patterns = this.detectEmotionalPatterns(emotionSequences)
    return {
      dominantEmotions,
      volatility,
      emotionalRange,
      patterns,
    }
  }
  public generateEmotionalResponseParameters(targetEmotion?: EmotionProfile): {
    tone: string
    intensity: number
    empathyLevel: number
    suggestedPhrases: string[]
  } {
    const emotionProfile = targetEmotion || this.currentEmotionProfile
    const tone = this.determineResponseTone(emotionProfile)
    const intensity = this.calculateResponseIntensity(emotionProfile)
    const empathyLevel = this.determineEmpathyLevel(emotionProfile)
    const suggestedPhrases = this.generateEmotionalPhrases(emotionProfile)
    return {
      tone,
      intensity,
      empathyLevel,
      suggestedPhrases,
    }
  }
  private createNeutralProfile(): EmotionProfile {
    const neutralProfile: EmotionProfile = {
      primaryEmotions: {
        [EmotionDimension.JOY]: 0.1,
        [EmotionDimension.SADNESS]: 0.1,
        [EmotionDimension.ANGER]: 0.1,
        [EmotionDimension.FEAR]: 0.1,
        [EmotionDimension.DISGUST]: 0.1,
        [EmotionDimension.SURPRISE]: 0.1,
        [EmotionDimension.TRUST]: 0.2,
        [EmotionDimension.ANTICIPATION]: 0.2,
      },
      secondaryEmotions: {},
      valence: 0.1, 
      arousal: 0.2, 
      confidence: ConfidenceLevel.HIGH,
      contextFactors: [],
    }
    this.calculateSecondaryEmotions(neutralProfile)
    return neutralProfile
  }
  private extractEmotionsFromText(text: string): {
    [key in EmotionDimension]?: number
  } {
    const extractedEmotions: { [key in EmotionDimension]?: number } = {}
    const lowerText = text.toLowerCase()
    const emotionLexicon: { [key in EmotionDimension]: string[] } = {
      [EmotionDimension.JOY]: [
        'happy',
        'joy',
        'delight',
        'pleased',
        'glad',
        'yay',
        'great',
        'excellent',
      ],
      [EmotionDimension.SADNESS]: [
        'sad',
        'unhappy',
        'disappointed',
        'sorry',
        'regret',
        'miss',
        'depressed',
      ],
      [EmotionDimension.ANGER]: [
        'angry',
        'upset',
        'mad',
        'annoyed',
        'irritated',
        'frustrating',
        'fury',
      ],
      [EmotionDimension.FEAR]: [
        'afraid',
        'scared',
        'fear',
        'terrified',
        'anxious',
        'worried',
        'nervous',
      ],
      [EmotionDimension.DISGUST]: [
        'disgust',
        'gross',
        'revolting',
        'nasty',
        'eww',
        'awful',
      ],
      [EmotionDimension.SURPRISE]: [
        'surprised',
        'shock',
        'astonished',
        'unexpected',
        'wow',
        'amazing',
      ],
      [EmotionDimension.TRUST]: [
        'trust',
        'believe',
        'faith',
        'confident',
        'reliable',
        'depend',
      ],
      [EmotionDimension.ANTICIPATION]: [
        'anticipate',
        'expect',
        'looking forward',
        'hope',
        'excited',
      ],
    }
    const intensifiers = ['very', 'extremely', 'really', 'so', 'incredibly']
    const diminishers = ['slightly', 'somewhat', 'a bit', 'a little']
    for (const [emotion, keywords] of Object.entries(emotionLexicon)) {
      let emotionScore = 0
      for (const keyword of keywords) {
        if (lowerText.includes(keyword)) {
          let intensity = 0.5
          for (const intensifier of intensifiers) {
            if (lowerText.includes(`${intensifier} ${keyword}`)) {
              intensity = 0.8
              break
            }
          }
          for (const diminisher of diminishers) {
            if (lowerText.includes(`${diminisher} ${keyword}`)) {
              intensity = 0.3
              break
            }
          }
          emotionScore = Math.max(emotionScore, intensity)
        }
      }
      if (emotionScore > 0.1) {
        extractedEmotions[emotion as EmotionDimension] = emotionScore
      }
    }
    if (Object.keys(extractedEmotions).length === 0) {
      extractedEmotions[EmotionDimension.TRUST] = 0.2
      extractedEmotions[EmotionDimension.ANTICIPATION] = 0.2
    }
    return extractedEmotions
  }
  private scaleEmotionIntensity(
    emotions: { [key in EmotionDimension]?: number },
    factor: number
  ): { [key in EmotionDimension]?: number } {
    const scaled: { [key in EmotionDimension]?: number } = {}
    for (const [emotion, intensity] of Object.entries(emotions)) {
      scaled[emotion as EmotionDimension] = Math.min(1, intensity * factor)
    }
    return scaled
  }
  private updateEmotionalState(
    newEmotions: { [key in EmotionDimension]?: number },
    contextFactors: string[] = [],
    trigger?: string
  ): void {
    for (const emotion of Object.values(EmotionDimension)) {
      if (this.currentEmotionProfile.primaryEmotions[emotion]) {
        this.currentEmotionProfile.primaryEmotions[emotion] =
          this.currentEmotionProfile.primaryEmotions[emotion]! *
          this.emotionDecayRate
      }
    }
    for (const [emotion, intensity] of Object.entries(newEmotions)) {
      const currentIntensity =
        this.currentEmotionProfile.primaryEmotions[
          emotion as EmotionDimension
        ] || 0
      this.currentEmotionProfile.primaryEmotions[emotion as EmotionDimension] =
        currentIntensity * (1 - this.sensitivityLevel) +
        intensity * this.sensitivityLevel
    }
    this.calculateSecondaryEmotions(this.currentEmotionProfile)
    this.calculateValenceArousal(this.currentEmotionProfile)
    this.updateDominantEmotions(this.currentEmotionProfile)
    this.currentEmotionProfile.contextFactors = contextFactors
    this.updateConfidenceLevel(this.currentEmotionProfile)
    this.emotionHistory.push({
      timestamp: Date.now(),
      profile: { ...this.currentEmotionProfile },
      trigger,
    })
    if (this.emotionHistory.length > 50) {
      this.emotionHistory = this.emotionHistory.slice(-50)
    }
  }
  private calculateSecondaryEmotions(profile: EmotionProfile): void {
    const primary = profile.primaryEmotions
    const secondary: { [key in SecondaryEmotion]?: number } = {}
    const joy = primary[EmotionDimension.JOY] || 0
    secondary[SecondaryEmotion.CONTENTMENT] = joy * 0.8
    secondary[SecondaryEmotion.HAPPINESS] = joy * 0.9
    secondary[SecondaryEmotion.AMUSEMENT] = joy * 0.6
    const anticipation = primary[EmotionDimension.ANTICIPATION] || 0
    secondary[SecondaryEmotion.OPTIMISM] =
      (joy * 0.6 + anticipation * 0.4) * 0.8
    const sadness = primary[EmotionDimension.SADNESS] || 0
    secondary[SecondaryEmotion.DISAPPOINTMENT] = sadness * 0.7
    secondary[SecondaryEmotion.GRIEF] = sadness * 0.9
    const fear = primary[EmotionDimension.FEAR] || 0
    secondary[SecondaryEmotion.ANXIETY] = fear * 0.7
    secondary[SecondaryEmotion.WORRY] = fear * 0.6
    const trust = primary[EmotionDimension.TRUST] || 0
    secondary[SecondaryEmotion.ACCEPTANCE] = trust * 0.8
    secondary[SecondaryEmotion.ADMIRATION] = trust * 0.7
    const anger = primary[EmotionDimension.ANGER] || 0
    const surprise = primary[EmotionDimension.SURPRISE] || 0
    secondary[SecondaryEmotion.COMPASSION] = (sadness * 0.4 + trust * 0.6) * 0.8
    secondary[SecondaryEmotion.JEALOUSY] = (fear * 0.5 + anger * 0.5) * 0.7
    secondary[SecondaryEmotion.CURIOSITY] = (trust * 0.4 + surprise * 0.6) * 0.8
    secondary[SecondaryEmotion.NOSTALGIA] = (joy * 0.5 + sadness * 0.5) * 0.7
    for (const [emotion, intensity] of Object.entries(secondary)) {
      if (intensity < 0.2) {
        delete secondary[emotion as SecondaryEmotion]
      }
    }
    profile.secondaryEmotions = secondary
  }
  private calculateValenceArousal(profile: EmotionProfile): void {
    const primary = profile.primaryEmotions
    const positiveValence =
      (primary[EmotionDimension.JOY] || 0) +
      (primary[EmotionDimension.TRUST] || 0) * 0.7 +
      (primary[EmotionDimension.ANTICIPATION] || 0) * 0.5
    const negativeValence =
      (primary[EmotionDimension.SADNESS] || 0) +
      (primary[EmotionDimension.ANGER] || 0) +
      (primary[EmotionDimension.FEAR] || 0) +
      (primary[EmotionDimension.DISGUST] || 0)
    const totalIntensity =
      positiveValence +
      negativeValence +
      (primary[EmotionDimension.SURPRISE] || 0)
    profile.valence =
      totalIntensity > 0
        ? (positiveValence - negativeValence) / totalIntensity
        : 0
    const highArousalContribution =
      (primary[EmotionDimension.ANGER] || 0) * 1.0 +
      (primary[EmotionDimension.FEAR] || 0) * 0.9 +
      (primary[EmotionDimension.SURPRISE] || 0) * 0.8 +
      (primary[EmotionDimension.JOY] || 0) * 0.6
    const lowArousalContribution =
      (primary[EmotionDimension.SADNESS] || 0) * 0.3 +
      (primary[EmotionDimension.TRUST] || 0) * 0.2
    profile.arousal =
      totalIntensity > 0
        ? (highArousalContribution - lowArousalContribution) / totalIntensity
        : 0.2
    profile.arousal = Math.max(0, Math.min(1, profile.arousal))
  }
  private updateDominantEmotions(profile: EmotionProfile): void {
    let maxPrimaryIntensity = 0
    let dominantPrimary: EmotionDimension | undefined
    for (const [emotion, intensity] of Object.entries(
      profile.primaryEmotions
    )) {
      if (intensity > maxPrimaryIntensity) {
        maxPrimaryIntensity = intensity
        dominantPrimary = emotion as EmotionDimension
      }
    }
    profile.dominantEmotion = dominantPrimary
    let maxSecondaryIntensity = 0
    let dominantSecondary: SecondaryEmotion | undefined
    for (const [emotion, intensity] of Object.entries(
      profile.secondaryEmotions
    )) {
      if (intensity > maxSecondaryIntensity) {
        maxSecondaryIntensity = intensity
        dominantSecondary = emotion as SecondaryEmotion
      }
    }
    profile.dominantSecondaryEmotion = dominantSecondary
  }
  private updateConfidenceLevel(profile: EmotionProfile): void {
    let totalIntensity = 0
    let emotionCount = 0
    for (const intensity of Object.values(profile.primaryEmotions)) {
      totalIntensity += intensity
      emotionCount++
    }
    const avgIntensity = emotionCount > 0 ? totalIntensity / emotionCount : 0
    let confidence: ConfidenceLevel
    if (emotionCount <= 1 && avgIntensity < 0.3) {
      confidence = ConfidenceLevel.VERY_LOW
    } else if (avgIntensity < 0.4) {
      confidence = ConfidenceLevel.LOW
    } else if (avgIntensity < 0.6) {
      confidence = ConfidenceLevel.MODERATE
    } else if (avgIntensity < 0.8) {
      confidence = ConfidenceLevel.HIGH
    } else {
      confidence = ConfidenceLevel.VERY_HIGH
    }
    profile.confidence = confidence
  }
  private initializePatternDictionary(): void {
    this.patternDictionary.set('gradual_calming', [3, 3, 3, 3, 0, 0]) 
    this.patternDictionary.set('intensifying_anger', [0, 0, 2, 2, 2, 2]) 
    this.patternDictionary.set('trust_betrayal', [6, 6, 6, 2, 2, 2]) 
    this.patternDictionary.set('surprise_to_joy', [5, 5, 0, 0, 0]) 
    this.patternDictionary.set('fear_to_relief', [3, 3, 3, 6, 0, 0]) 
  }
  private extractEmotionSequences(): number[][] {
    const emotionIndices: number[] = []
    for (const entry of this.emotionHistory) {
      const dominantEmotion = entry.profile.dominantEmotion
      if (dominantEmotion) {
        const emotionValues = Object.values(EmotionDimension)
        const index = emotionValues.indexOf(dominantEmotion)
        emotionIndices.push(index)
      }
    }
    const sequences: number[][] = []
    for (let i = 0; i <= emotionIndices.length - 3; i++) {
      sequences.push(emotionIndices.slice(i, i + 6)) 
    }
    return sequences
  }
  private calculateDominantEmotions(): [EmotionDimension, number][] {
    const emotionCounts = new Map<EmotionDimension, number>()
    for (const entry of this.emotionHistory) {
      const dominantEmotion = entry.profile.dominantEmotion
      if (dominantEmotion) {
        emotionCounts.set(
          dominantEmotion,
          (emotionCounts.get(dominantEmotion) || 0) + 1
        )
      }
    }
    const total = this.emotionHistory.length
    const dominantEmotions: [EmotionDimension, number][] = Array.from(
      emotionCounts.entries()
    )
      .map(([emotion, count]): [EmotionDimension, number] => [
        emotion,
        count / total,
      ])
      .sort((a, b): number => Number(b[1]) - Number(a[1]))
      .slice(0, 3) 
    return dominantEmotions
  }
  private calculateEmotionalVolatility(): number {
    if (this.emotionHistory.length < 3) return 0
    let changes = 0
    for (let i = 1; i < this.emotionHistory.length; i++) {
      const prevEmotion = this.emotionHistory[i - 1].profile.dominantEmotion
      const currEmotion = this.emotionHistory[i].profile.dominantEmotion
      if (prevEmotion !== currEmotion) {
        changes++
      }
    }
    return changes / (this.emotionHistory.length - 1)
  }
  private calculateEmotionalRange(): number {
    const uniqueEmotions = new Set<EmotionDimension>()
    for (const entry of this.emotionHistory) {
      if (entry.profile.dominantEmotion) {
        uniqueEmotions.add(entry.profile.dominantEmotion)
      }
    }
    return uniqueEmotions.size / Object.keys(EmotionDimension).length
  }
  private detectEmotionalPatterns(sequences: number[][]): string[] {
    const detectedPatterns = new Set<string>()
    for (const sequence of sequences) {
      for (const [
        patternName,
        patternSequence,
      ] of this.patternDictionary.entries()) {
        const similarity = this.calculateSequenceSimilarity(
          sequence,
          patternSequence
        )
        if (similarity > 0.7) {
          detectedPatterns.add(patternName)
        }
      }
    }
    return Array.from(detectedPatterns)
  }
  private calculateSequenceSimilarity(seq1: number[], seq2: number[]): number {
    const minLength = Math.min(seq1.length, seq2.length)
    if (minLength === 0) return 0
    let matches = 0
    for (let i = 0; i < minLength; i++) {
      if (seq1[i] === seq2[i]) {
        matches++
      }
    }
    return matches / minLength
  }
  private determineResponseTone(profile: EmotionProfile): string {
    const dominantEmotion = profile.dominantEmotion
    const valence = profile.valence
    const arousal = profile.arousal
    let tone = 'neutral'
    if (dominantEmotion) {
      switch (dominantEmotion) {
        case EmotionDimension.JOY:
          tone = 'cheerful'
          break
        case EmotionDimension.SADNESS:
          tone = 'gentle'
          break
        case EmotionDimension.ANGER:
          tone = 'calm'
          break
        case EmotionDimension.FEAR:
          tone = 'reassuring'
          break
        case EmotionDimension.DISGUST:
          tone = 'understanding'
          break
        case EmotionDimension.SURPRISE:
          tone = 'curious'
          break
        case EmotionDimension.TRUST:
          tone = 'warm'
          break
        case EmotionDimension.ANTICIPATION:
          tone = 'enthusiastic'
          break
      }
    }
    if (valence < -0.5) {
      tone = arousal > 0.7 ? 'soothing' : 'compassionate'
    } else if (valence > 0.5 && arousal > 0.7) {
      tone = 'enthusiastic'
    }
    return tone
  }
  private calculateResponseIntensity(profile: EmotionProfile): number {
    const baseIntensity = profile.arousal
    const valence = profile.valence
    if (valence < -0.5 && baseIntensity > 0.6) {
      return baseIntensity * 0.7
    } else if (valence > 0.5) {
      return Math.min(1, baseIntensity * 1.2)
    }
    return baseIntensity
  }
  private determineEmpathyLevel(profile: EmotionProfile): number {
    let empathy = this.empathyLevel
    const dominantEmotion = profile.dominantEmotion
    if (dominantEmotion) {
      switch (dominantEmotion) {
        case EmotionDimension.SADNESS:
        case EmotionDimension.FEAR:
          empathy = Math.min(1, empathy * 1.2)
          break
        case EmotionDimension.ANGER:
        case EmotionDimension.DISGUST:
          empathy = empathy * 0.9
          break
      }
    }
    return empathy
  }
  private generateEmotionalPhrases(profile: EmotionProfile): string[] {
    const dominantEmotion = profile.dominantEmotion
    const valence = profile.valence
    const phrases: string[] = [
      'I understand',
      'That makes sense',
      'I see what you mean',
    ]
    if (dominantEmotion) {
      switch (dominantEmotion) {
        case EmotionDimension.JOY:
          phrases.push(
            "That's wonderful!",
            "I'm happy to hear that",
            'That sounds delightful'
          )
          break
        case EmotionDimension.SADNESS:
          phrases.push(
            "I'm sorry to hear that",
            'That must be difficult',
            "It's okay to feel this way"
          )
          break
        case EmotionDimension.ANGER:
          phrases.push(
            'I understand your frustration',
            'That would be upsetting',
            "Let's work through this together"
          )
          break
        case EmotionDimension.FEAR:
          phrases.push(
            "It's natural to feel concerned",
            "I'm here to help with that",
            "Let's think about this calmly"
          )
          break
        case EmotionDimension.SURPRISE:
          phrases.push(
            "That's unexpected!",
            'I can see why that would surprise you',
            'What an interesting development'
          )
          break
        case EmotionDimension.TRUST:
          phrases.push(
            'I value your perspective',
            'Thank you for sharing that with me',
            'I appreciate your openness'
          )
          break
      }
    }
    if (valence < -0.3) {
      phrases.push(
        'Things may improve with time',
        'Would it help to talk more about this?',
        "I'm here to listen"
      )
    } else if (valence > 0.3) {
      phrases.push(
        "That's really positive",
        "I'm glad to hear things are going well",
        "It's nice to share good moments"
      )
    }
    return phrases
  }
}