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
export interface CharacterBookExtensions extends Record<string, unknown> {}
export interface CharacterBookEntryExtensions extends Record<string, unknown> {}