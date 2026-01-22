export interface Extensions extends Record<string, unknown> {
  depth_prompt?: ExtensionsDepthPrompt
  fav?: boolean
  talkativeness?: number
  world?: string
}
export interface ExtensionsDepthPrompt {
  depth: number
  prompt: string
  role: 'system' | ({} & string)
}