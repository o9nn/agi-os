export type ParseResult
  = | { command: 'post tweet', content: string }
    | { command: 'search tweets', content: string }
    | { command: 'like tweet', content: string }
    | { command: 'retweet', content: string }
    | { command: 'get user', content: string }
    | { command: 'get timeline', content: string, count: number }
type NonTimelineCommands = 'post tweet' | 'search tweets' | 'like tweet' | 'retweet' | 'get user'
export function parseTwitterCommand(input: string): ParseResult | null {
  const normalizedInput = input.trim().toLowerCase()
  const commandPatterns: Array<{ pattern: string, command: string }> = [
    { pattern: 'post tweet:', command: 'post tweet' },
    { pattern: 'search tweets:', command: 'search tweets' },
    { pattern: 'like tweet:', command: 'like tweet' },
    { pattern: 'retweet:', command: 'retweet' },
    { pattern: 'get user:', command: 'get user' },
    { pattern: 'get timeline', command: 'get timeline' },
  ]
  for (const { pattern, command } of commandPatterns) {
    if (normalizedInput.startsWith(pattern)) {
      const content = input.substring(pattern.length)
      if (command === 'get timeline') {
        const countMatch = content.match(/count:\s*(\d+)/)
        const count = countMatch ? Number.parseInt(countMatch[1], 10) : 10
        const trimmedContent = content.trim()
        return { command: command as ParseResult['command'], content: trimmedContent, count }
      }
      return { command: command as NonTimelineCommands, content: content.trim() }
    }
  }
  return null
}