/**
 * Echo Character Package
 * 
 * Main exports for Echo - The Living Memory System character implementation
 */

export { EchoCharacter } from './character'
export type { EchoConfig, EchoReflection, CognitiveState } from './config'
export { defaultEchoConfig, initialCognitiveState } from './config'
export {
  ECHO_SYSTEM_PROMPT,
  ECHO_COGNITIVE_INSTRUCTIONS,
  ECHO_REFLECTION_TEMPLATE,
} from './prompts'
export type { EchoWebSocketEvents, EchoEvent } from './types'
