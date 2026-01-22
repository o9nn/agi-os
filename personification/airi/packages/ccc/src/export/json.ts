import type { Card } from '../define'
import type { CharacterCardV3 } from './types/character_card_v3'
export function exportToJSON(data: Card): CharacterCardV3 {
  return {
    spec: 'chara_card_v3',
    spec_version: '3.0',
    data: createCardData(data),
  }
}
function createCardData(data: Card): CharacterCardV3['data'] {
  return {
    name: data.name,
    nickname: data.nickname,
    description: data.description ?? '',
    personality: data.personality ?? '',
    scenario: data.scenario ?? '',
    first_mes: data.greetings?.[0] ?? '',
    alternate_greetings: data.greetings?.slice(1) ?? [],
    group_only_greetings: data.greetingsGroupOnly ?? [],
    character_version: data.version,
    creator: data.creator ?? '',
    creator_notes: data.notes ?? '',
    creator_notes_multilingual: data.notesMultilingual,
    system_prompt: data.systemPrompt ?? '',
    post_history_instructions: data.postHistoryInstructions ?? '',
    mes_example: formatMessageExample(data.messageExample),
    tags: data.tags ?? [],
    extensions: createExtensions(data),
  }
}
function formatMessageExample(messageExample: string[][] | undefined): string {
  if (!messageExample)
    return ''
  return messageExample
    .map(arr => `<START>\n${arr.join('\n')}`)
    .join('\n')
}
function createExtensions(data: Card): Record<string, any> {
  return {
    depth_prompt: {
      depth: 4,
      prompt: '',
      role: 'system',
    },
    fav: false,
    talkativeness: 0.5,
    ...data.extensions,
  }
}