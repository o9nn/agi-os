export interface Asset {
  ext: string
  name: string
  type: string
  uri: string
}
export type Assets = Asset[]
export interface CharacterBook {
  description?: string
  entries: CharacterBookEntry[]
  extensions: CharacterBookExtensions
  name?: string
  recursive_scanning?: boolean
  scan_depth?: number
  token_budget?: number
}
export interface CharacterBookEntry {
  case_sensitive?: boolean
  comment?: string
  constant?: boolean
  content: string
  enabled: boolean
  extensions: CharacterBookEntryExtensions
  id?: number
  insertion_order: number
  keys: string[]
  name?: string
  position?: 'after_char' | 'before_char'
  priority?: number
  secondary_keys?: string[]
  selective?: boolean
}
export interface CharacterBookEntryExtensions extends Record<string, unknown> {
}
export interface CharacterBookExtensions extends Record<string, unknown> {
}
export interface CharacterCardV3 {
  data: Data
  spec: 'chara_card_v3'
  spec_version: '3.0'
}
export type Data = DataV1 & DataV2 & DataV3
export interface DataV1 {
  description: string
  first_mes: string
  mes_example: string
  name: string
  personality: string
  scenario: string
}
export interface DataV2 {
  alternate_greetings: string[]
  character_book?: CharacterBook
  character_version: string
  creator: string
  creator_notes: string
  extensions: Extensions
  post_history_instructions: string
  system_prompt: string
  tags: string[]
}
export interface DataV3 {
  assets?: Assets
  creation_date?: number
  creator_notes_multilingual?: Record<string, string>
  group_only_greetings: string[]
  modification_date?: number
  nickname?: string
  source?: string[]
}
export interface Extensions extends Record<string, unknown> {
  depth_prompt?: ExtensionsDepthPrompt
  fav?: boolean
  talkativeness?: number
  world?: string
}
export interface ExtensionsDepthPrompt {
  depth: number
  prompt: string
  role: 'system' | (string & {})
}
export interface Message {
  content: string
  memo?: string
  name?: string
  role: 'assistant' | 'function' | 'system' | 'user'
}