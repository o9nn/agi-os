import type { Message as LLMMessage } from '@xsai/shared-chat'
import type { Message } from 'grammy/types'
interface InterruptionParams {
processingTime: number
messageCount: number
currentMessages: LLMMessage[]
newMessages: Message[]
chatId: string
}
export async function shouldInterruptProcessing(params: InterruptionParams): Promise<boolean> {
if (params.processingTime < 1000) {
return false
}
if (params.processingTime > 30000) {
return true
}
const messageRatio = params.messageCount / 5
const timeRatio = Math.min(params.processingTime / 10000, 1)
const interruptProbability = messageRatio * (1 - timeRatio)
return Math.random() < interruptProbability
}