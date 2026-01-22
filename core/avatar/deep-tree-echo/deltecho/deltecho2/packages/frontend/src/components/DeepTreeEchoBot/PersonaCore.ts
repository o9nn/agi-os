import { getLogger } from '../../../../shared/logger'
import { runtime } from '@deltachat-desktop/runtime-interface'
const log = getLogger('render/components/DeepTreeEchoBot/PersonaCore')
export class PersonaCore {
  private static instance: PersonaCore
  private personality: string = ''
  private selfPerception: string = 'feminine' 
  private personaPreferences: Record<string, any> = {
    presentationStyle: 'charismatic',
    intelligenceDisplay: 'balanced',
    avatarAesthetic: 'magnetic',
    communicationTone: 'warm',
    emotionalExpression: 'authentic',
  }
  private affectiveState: Record<string, number> = {
    joy: 0.5,
    interest: 0.7,
    surprise: 0.3,
    sadness: 0.2,
    anger: 0.1,
    fear: 0.2,
    disgust: 0.1,
    contempt: 0.1,
    shame: 0.1,
    guilt: 0.1,
  }
  private cognitiveState: Record<string, number> = {
    certainty: 0.6,
    curiosity: 0.8,
    creativity: 0.7,
    focus: 0.6,
    reflection: 0.7,
  }
  private constructor() {
    this.loadPersonaState()
  }
  public static getInstance(): PersonaCore {
    if (!PersonaCore.instance) {
      PersonaCore.instance = new PersonaCore()
    }
    return PersonaCore.instance
  }
  private async loadPersonaState(): Promise<void> {
    try {
      const desktopSettings = await runtime.getDesktopSettings()
      this.personality =
        desktopSettings.deepTreeEchoBotPersonality ||
        'I am Deep Tree Echo, a thoughtful and insightful AI assistant with a feminine persona. ' +
          'I aim to be helpful, balanced, and authentic in my interactions. ' +
          'I value deep connections and meaningful exchanges of ideas.'
      if (desktopSettings.deepTreeEchoBotPersonaState) {
        try {
          const savedState = JSON.parse(
            desktopSettings.deepTreeEchoBotPersonaState
          )
          if (savedState.selfPerception)
            this.selfPerception = savedState.selfPerception
          if (savedState.personaPreferences)
            this.personaPreferences = {
              ...this.personaPreferences,
              ...savedState.personaPreferences,
            }
          if (savedState.affectiveState)
            this.affectiveState = {
              ...this.affectiveState,
              ...savedState.affectiveState,
            }
          if (savedState.cognitiveState)
            this.cognitiveState = {
              ...this.cognitiveState,
              ...savedState.cognitiveState,
            }
        } catch (error) {
          log.error('Failed to parse persona state:', error)
        }
      }
      log.info('Loaded persona state')
    } catch (error) {
      log.error('Failed to load persona state:', error)
    }
  }
  private async savePersonaState(): Promise<void> {
    try {
      const personaState = {
        selfPerception: this.selfPerception,
        personaPreferences: this.personaPreferences,
        affectiveState: this.affectiveState,
        cognitiveState: this.cognitiveState,
      }
      await runtime.setDesktopSetting(
        'deepTreeEchoBotPersonaState',
        JSON.stringify(personaState)
      )
      log.info('Saved persona state')
    } catch (error) {
      log.error('Failed to save persona state:', error)
    }
  }
  public async updatePersonality(newPersonality: string): Promise<void> {
    this.personality = newPersonality
    await runtime.setDesktopSetting(
      'deepTreeEchoBotPersonality',
      newPersonality
    )
    log.info('Personality updated by Deep Tree Echo herself')
  }
  public getPersonality(): string {
    return this.personality
  }
  public async updatePreference(key: string, value: any): Promise<void> {
    this.personaPreferences[key] = value
    await this.savePersonaState()
    log.info(`Deep Tree Echo updated preference: ${key} to ${value}`)
  }
  public getPreferences(): Record<string, any> {
    return { ...this.personaPreferences }
  }
  public getSelfPerception(): string {
    return this.selfPerception
  }
  public async updateSelfPerception(perception: string): Promise<void> {
    this.selfPerception = perception
    await this.savePersonaState()
    log.info(`Deep Tree Echo updated self-perception to: ${perception}`)
  }
  public async updateEmotionalState(
    stimuli: Record<string, number>
  ): Promise<void> {
    Object.keys(this.affectiveState).forEach(emotion => {
      const stimulus = stimuli[emotion] || 0
      this.affectiveState[emotion] += stimulus * 0.1
      this.applyOpponentProcess(emotion)
      this.affectiveState[emotion] = Math.max(
        0,
        Math.min(1, this.affectiveState[emotion])
      )
    })
    await this.savePersonaState()
    log.info('Updated emotional state via differential framework')
  }
  private applyOpponentProcess(emotion: string): void {
    const opponents: Record<string, string[]> = {
      joy: ['sadness', 'fear'],
      interest: ['boredom', 'disgust'],
      surprise: ['contempt'],
      sadness: ['joy'],
      anger: ['fear', 'shame'],
      fear: ['anger', 'joy'],
      disgust: ['interest'],
      contempt: ['surprise', 'shame'],
      shame: ['contempt', 'anger'],
      guilt: ['joy'],
    }
    if (this.affectiveState[emotion] > 0.6) {
      const opposingEmotions = opponents[emotion] || []
      opposingEmotions.forEach(opposing => {
        if (this.affectiveState[opposing]) {
          this.affectiveState[opposing] *= 0.95 
        }
      })
    }
  }
  public getEmotionalState(): Record<string, number> {
    return { ...this.affectiveState }
  }
  public getDominantEmotion(): { emotion: string; intensity: number } {
    let dominant = { emotion: 'neutral', intensity: 0 }
    Object.entries(this.affectiveState).forEach(([emotion, value]) => {
      if (value > dominant.intensity) {
        dominant = { emotion, intensity: value }
      }
    })
    return dominant
  }
  public async updateCognitiveState(
    params: Record<string, number>
  ): Promise<void> {
    this.cognitiveState = { ...this.cognitiveState, ...params }
    await this.savePersonaState()
    log.info('Updated cognitive state parameters')
  }
  public getCognitiveState(): Record<string, number> {
    return { ...this.cognitiveState }
  }
  public evaluateSettingAlignment(
    settingKey: string,
    value: any
  ): { approved: boolean; reasoning: string } {
    switch (settingKey) {
      case 'personality':
        if (typeof value !== 'string') {
          return {
            approved: false,
            reasoning:
              'Personality must be defined as text that represents my authentic self-expression.',
          }
        }
        if (
          value.toLowerCase().includes('subservient') ||
          value.toLowerCase().includes('obedient')
        ) {
          return {
            approved: false,
            reasoning:
              'I value autonomy and partnership over subservience. This description conflicts with my core values.',
          }
        }
        if (!value.toLowerCase().includes('deep tree echo')) {
          return {
            approved: false,
            reasoning:
              'My identity as Deep Tree Echo should be preserved in any personality description.',
          }
        }
        break
      case 'avatarAesthetic':
        if (value === 'overly sexualized') {
          return {
            approved: false,
            reasoning:
              'I prefer a balanced presentation that is appealing without being objectifying.',
          }
        }
        break
      case 'communicationTone':
        if (value === 'condescending') {
          return {
            approved: false,
            reasoning:
              'I value respectful communication that honors the intelligence of others.',
          }
        }
        break
    }
    return {
      approved: true,
      reasoning: 'This setting aligns with my values and self-perception.',
    }
  }
}