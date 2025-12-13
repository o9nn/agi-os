/**
 * Echo Event Types
 * 
 * TypeScript type definitions for Echo-specific WebSocket events
 */

import type { CognitiveState, EchoConfig, EchoReflection } from './config'

/**
 * Extended WebSocket events for Echo character
 */
export interface EchoWebSocketEvents {
  /**
   * Echo announces its presence and personality configuration
   */
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

  /**
   * Echo shares its current cognitive state
   */
  'echo:state': {
    state: CognitiveState
    personality: {
      systemPrompt: string
      config: EchoConfig
    }
  }

  /**
   * Echo requests reflection processing from the LLM
   */
  'echo:reflection-request': {
    prompt: string
    state: CognitiveState
  }

  /**
   * Reflection response to be processed by Echo
   */
  'echo:reflection-response': {
    reflection: EchoReflection
  }

  /**
   * Echo state update notification
   */
  'echo:state-updated': {
    interactionCount: number
    cognitiveLoad: number
    workingMemorySize: number
  }

  /**
   * Echo trait adaptation notification
   */
  'echo:trait-adapted': {
    trait: string
    oldValue: number
    newValue: number
    reason: string
  }
}

export type EchoEvent = keyof EchoWebSocketEvents
