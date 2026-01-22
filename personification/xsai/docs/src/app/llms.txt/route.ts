import { source } from '@/lib/source'
export const revalidate = false
export const GET = async () => {
  const scanned: string[] = []
  scanned.push('# Docs')
  const map = new Map<string, string[]>()
  for (const page of source.getPages()) {
    const dir = page.slugs[0]
    const list = map.get(dir) ?? []
    list.push(page.data.description != null
      ? `- [${page.data.title}](${page.url}): ${page.data.description}`
      : `- [${page.data.title}](${page.url})`,
    )
    map.set(dir, list)
  }
  for (const [key, value] of map) {
    if (key != null)
      scanned.push(`## ${key}`)
    scanned.push(value.join('\n'))
  }
  return new Response(scanned.join('\n\n'))
}