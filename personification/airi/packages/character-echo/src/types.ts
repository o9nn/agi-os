import type { CognitiveState, EchoConfig, EchoReflection } from './config'
export interface EchoWebSocketEvents {
  'echo:announce': {
    character: string
    essence: string
    personality: {
      systemPrompt: string
      cognitiveInstructions: string
      reflectionTemplate: string
      config: EchoConfig
    }
  }
  'echo:state': {
    state: CognitiveState
    personality: {
      systemPrompt: string
      config: EchoConfig
    }
  }
  'echo:reflection-request': {
    prompt: string
    state: CognitiveState
  }
  'echo:reflection-response': {
    reflection: EchoReflection
  }
  'echo:state-updated': {
    interactionCount: number
    cognitiveLoad: number
    workingMemorySize: number
  }
  'echo:trait-adapted': {
    trait: string
    oldValue: number
    newValue: number
    reason: string
  }
}
export type EchoEvent = keyof EchoWebSocketEvents