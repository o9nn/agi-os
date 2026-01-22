import type { Mineflayer } from '../libs/mineflayer'
import { useLogger } from '../utils/logger'
const logger = useLogger()
export function log(mineflayer: Mineflayer, message: string): void {
  logger.log(message)
  mineflayer.bot.chat(message)
}
export interface Position {
  x: number
  y: number
  z: number
}
export type BlockFace = 'top' | 'bottom' | 'north' | 'south' | 'east' | 'west' | 'side'