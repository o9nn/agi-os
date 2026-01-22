import { env } from 'node:process'
import { useLogg } from '@guiiai/logg'
import { Client as AiriClient } from '@proj-airi/server-sdk'
import { EchoCharacter, type EchoReflection } from '@proj-airi/character-echo'
const log = useLogg('EchoService')
export interface EchoServiceConfig {
  airiToken?: string
  airiUrl?: string
  enableAutoReflection?: boolean
}
export class EchoService {
  private airiClient: AiriClient
  private echo: EchoCharacter
  private config: EchoServiceConfig
  constructor(config: EchoServiceConfig = {}) {
    this.config = {
      enableAutoReflection: true,
      ...config,
    }
    this.echo = new EchoCharacter()
    this.airiClient = new AiriClient({
      name: 'echo-character',
      possibleEvents: [
        'input:text',
        'ui:configure',
      ],
      token: config.airiToken || env.AUTHENTICATION_TOKEN,
      url: config.airiUrl || env.AIRI_URL || 'ws://localhost:6121/ws',
    })
    this.setupEventHandlers()
    log.log('Echo service initialized')
  }
  private setupEventHandlers(): void {
    this.airiClient.onEvent('input:text', async (event) => {
      const input = event.data.text
      log.log('Received input:', input)
      const result = this.echo.processInput(input)
      log.log('Cognitive processing result:', {
        cognitiveLoad: result.cognitiveLoad,
        shouldReflect: result.shouldReflect,
      })
      const personality = this.echo.getPersonality()
      this.airiClient.send({
        type: 'echo:state',
        data: {
          state: this.echo.getState(),
          personality: {
            systemPrompt: personality.systemPrompt,
            config: personality.config,
          },
        },
      })
      if (result.shouldReflect && this.config.enableAutoReflection) {
        log.log('Triggering reflection...')
        this.triggerReflection()
      }
    })
    this.airiClient.onEvent('ui:configure', async (event) => {
      if (event.data.moduleName === 'echo') {
        log.log('Received configuration:', event.data.config)
        if (event.data.config && typeof event.data.config === 'object') {
          const config = event.data.config as Record<string, unknown>
          if (typeof config.enableAutoReflection === 'boolean') {
            this.config.enableAutoReflection = config.enableAutoReflection
            log.log('Auto-reflection set to:', config.enableAutoReflection)
          }
        }
      }
    })
  }
  private triggerReflection(): void {
    const reflectionPrompt = this.echo.generateReflectionPrompt()
    this.airiClient.send({
      type: 'echo:reflection-request',
      data: {
        prompt: reflectionPrompt,
        state: this.echo.getState(),
      },
    })
    log.log('Reflection request sent to AIRI system')
  }
  processReflection(reflection: EchoReflection): void {
    this.echo.addReflection(reflection)
    log.log('Reflection added to Echo state')
    if (reflection.how_did_i_adapt) {
      log.log('Echo adapted based on reflection')
    }
  }
  getPersonality() {
    return this.echo.getPersonality()
  }
  getState() {
    return this.echo.getState()
  }
  async start(): Promise<void> {
    log.log('Starting Echo service...')
    try {
      await this.airiClient.connect()
      this.airiClient.send({
        type: 'echo:announce',
        data: {
          character: 'Echo',
          essence: this.echo.getConfig().essence,
          personality: this.echo.getPersonality(),
        },
      })
      log.log('Echo service started successfully')
      log.log('Echo essence:', this.echo.getConfig().essence)
    }
    catch (error) {
      log.withError(error as Error).error('Failed to start Echo service')
      throw error
    }
  }
  async stop(): Promise<void> {
    log.log('Stopping Echo service...')
    try {
      this.airiClient.close()
      log.log('Echo service stopped')
    }
    catch (error) {
      log.withError(error as Error).error('Error stopping Echo service')
      throw error
    }
  }
}