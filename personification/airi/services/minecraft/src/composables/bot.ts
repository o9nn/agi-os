import type { MineflayerOptions } from '../libs/mineflayer'
import { Mineflayer } from '../libs/mineflayer'
let botInstance: Mineflayer | null = null
export async function initBot(options: MineflayerOptions): Promise<{ bot: Mineflayer }> {
if (botInstance) {
throw new Error('Bot already initialized')
}
botInstance = await Mineflayer.asyncBuild(options)
return { bot: botInstance }
}
export function useBot(): { bot: Mineflayer } {
if (!botInstance) {
throw new Error('Bot not initialized')
}
return {
bot: botInstance,
}
}