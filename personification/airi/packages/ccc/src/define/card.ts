import type { Data } from '../export/types'
import type { Message } from './types/mes_example'
interface CardCore {
creator?: Data['creator']
name: Data['name']
nickname?: Data['nickname']
version: Data['character_version']
}
interface CardMeta {
metadata?: Record<string, boolean | number | string>
}
interface CardAdditional {
extensions?: Data['extensions']
greetings?: string[]
greetingsGroupOnly?: string[]
notes?: Data['creator_notes']
notesMultilingual?: Data['creator_notes_multilingual']
}
interface CardDescription {
description?: string
}
interface CardExtra {
personality?: string
scenario?: string
systemPrompt?: string
postHistoryInstructions?: string
tags?: string[]
messageExample?: Message[][]
}
export type Card = CardAdditional & CardCore & CardDescription & CardMeta & CardExtra
export type CardFn<T extends Record<string, unknown> = Record<string, unknown>> = (data: T) => Card
export const defineCard = (card: Card) => card
export const defineCardFn = <T extends Record<string, unknown> = Record<string, unknown>>(card: CardFn<T>, data: T) => card(data)