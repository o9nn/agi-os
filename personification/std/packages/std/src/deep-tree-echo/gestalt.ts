import type { Echo, GestaltPattern } from './types'

export function createGestalt(
  components: string[],
  emergentProperties: Record<string, unknown>,
  options?: {
    id?: string
    coherence?: number
  },
): GestaltPattern {
  return {
    id: options?.id ?? crypto.randomUUID(),
    components,
    emergentProperties,
    coherence: options?.coherence ?? calculateCoherence(components),
  }
}

function calculateCoherence(components: string[]): number {
  if (components.length === 0)
    return 0
  if (components.length === 1)
    return 1
  return Math.max(0.1, 1 / Math.sqrt(components.length))
}

export function discoverPatterns(
  echoes: Echo[],
  minConnections = 2,
  minCoherence = 0.5,
): GestaltPattern[] {
  const patterns: GestaltPattern[] = []
  const echoMap = new Map(echoes.map(e => [e.id, e]))
  const visited = new Set<string>()

  for (const echo of echoes) {
    if (visited.has(echo.id))
      continue

    const cluster = findConnectedCluster(echo, echoMap, visited)

    if (cluster.length >= minConnections) {
      const coherence = calculateClusterCoherence(cluster, echoMap)

      if (coherence >= minCoherence) {
        patterns.push(
          createGestalt(
            cluster,
            { discovered: true },
            { coherence },
          ),
        )
      }
    }
  }

  return patterns
}

function findConnectedCluster(
  start: Echo,
  echoMap: Map<string, Echo>,
  visited: Set<string>,
): string[] {
  const cluster: string[] = []
  const stack = [start.id]

  while (stack.length > 0) {
    const id = stack.pop()!
    if (visited.has(id))
      continue

    visited.add(id)
    cluster.push(id)

    const echo = echoMap.get(id)
    if (echo) {
      for (const connId of echo.connections) {
        if (!visited.has(connId) && echoMap.has(connId)) {
          stack.push(connId)
        }
      }
    }
  }

  return cluster
}

function calculateClusterCoherence(
  cluster: string[],
  echoMap: Map<string, Echo>,
): number {
  if (cluster.length < 2)
    return 1

  let totalConnections = 0
  for (const id of cluster) {
    const echo = echoMap.get(id)
    if (echo) {
      const internalConnections = echo.connections.filter(c =>
        cluster.includes(c),
      ).length
      totalConnections += internalConnections
    }
  }

  totalConnections /= 2
  const maxConnections = (cluster.length * (cluster.length - 1)) / 2
  return maxConnections > 0 ? totalConnections / maxConnections : 0
}

export function mergePatterns(
  pattern1: GestaltPattern,
  pattern2: GestaltPattern,
): GestaltPattern {
  const combinedComponents = [
    ...new Set([...pattern1.components, ...pattern2.components]),
  ]

  return createGestalt(
    combinedComponents,
    {
      ...pattern1.emergentProperties,
      ...pattern2.emergentProperties,
      merged: true,
    },
    {
      coherence: (pattern1.coherence + pattern2.coherence) / 2,
    },
  )
}

export function strengthenPattern(
  pattern: GestaltPattern,
  amount: number,
): GestaltPattern {
  return {
    ...pattern,
    coherence: Math.min(1, pattern.coherence + amount),
  }
}

export function isPartOfPattern(
  echoId: string,
  pattern: GestaltPattern,
): boolean {
  return pattern.components.includes(echoId)
}
