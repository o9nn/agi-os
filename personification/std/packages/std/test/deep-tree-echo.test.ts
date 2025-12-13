import { describe, expect, it } from 'vitest'
import {
  calculateRelevance,
  connectEchoes,
  createAdaptiveTrait,
  createEcho,
  createGestalt,
  createNavigation,
  createWaysOfKnowing,
  decayRelevance,
  getMostRelevant,
  isPartOfPattern,
  reinforceEcho,
  updateNavigation,
} from '../src/deep-tree-echo'

describe('Deep Tree Echo - Echo Management', () => {
  it('should create an echo with default values', () => {
    const content = { message: 'Test' }
    const echo = createEcho(content)

    expect(echo.content).toBe(content)
    expect(echo.relevance).toBe(0.5)
    expect(echo.valence).toBe(0)
    expect(echo.connections).toEqual([])
    expect(echo.id).toBeDefined()
    expect(echo.timestamp).toBeGreaterThan(0)
  })

  it('should calculate relevance with equal weights', () => {
    const relevance = calculateRelevance({
      novelty: 0.8,
      emotional: 0.6,
      practical: 0.4,
      coherence: 1.0,
    })

    expect(relevance).toBe(0.7)
  })

  it('should create bidirectional connections', () => {
    const echo1 = createEcho('A', { id: 'echo-1' })
    const echo2 = createEcho('B', { id: 'echo-2' })

    const [updated1, updated2] = connectEchoes(echo1, echo2)

    expect(updated1.connections).toContain('echo-2')
    expect(updated2.connections).toContain('echo-1')
  })

  it('should filter and sort by relevance', () => {
    const echoes = [
      createEcho('A', { relevance: 0.3 }),
      createEcho('B', { relevance: 0.8 }),
      createEcho('C', { relevance: 0.6 }),
      createEcho('D', { relevance: 0.9 }),
    ]

    const relevant = getMostRelevant(echoes, 0.5)

    expect(relevant).toHaveLength(3)
    expect(relevant[0].relevance).toBe(0.9)
  })

  it('should decay relevance over time', () => {
    const echo = createEcho('test', { relevance: 1.0 })
    const futureTime = echo.timestamp + 1000000

    const decayed = decayRelevance(echo, 0.0001, futureTime)

    expect(decayed.relevance).toBeLessThan(echo.relevance)
  })

  it('should reinforce echo relevance', () => {
    const echo = createEcho('test', { relevance: 0.5 })
    const reinforced = reinforceEcho(echo, 0.3)

    expect(reinforced.relevance).toBe(0.8)
  })
})

describe('Deep Tree Echo - Adaptability', () => {
  it('should create trait with default 15% bounds', () => {
    const trait = createAdaptiveTrait(0.8)

    expect(trait.value).toBe(0.8)
    expect(trait.baseline).toBe(0.8)
    expect(trait.min).toBeCloseTo(0.68, 2)
    expect(trait.max).toBeCloseTo(0.92, 2)
  })
})

describe('Deep Tree Echo - Gestalt', () => {
  it('should create a gestalt pattern', () => {
    const pattern = createGestalt(
      ['echo-1', 'echo-2', 'echo-3'],
      { insight: 'Test insight' },
    )

    expect(pattern.components).toEqual(['echo-1', 'echo-2', 'echo-3'])
    expect(pattern.emergentProperties).toEqual({ insight: 'Test insight' })
    expect(pattern.id).toBeDefined()
    expect(pattern.coherence).toBeGreaterThan(0)
  })

  it('should detect when echo is in pattern', () => {
    const pattern = createGestalt(['e1', 'e2', 'e3'], {})
    expect(isPartOfPattern('e2', pattern)).toBe(true)
    expect(isPartOfPattern('e4', pattern)).toBe(false)
  })
})

describe('Deep Tree Echo - Wisdom', () => {
  it('should create balanced ways with no overrides', () => {
    const knowing = createWaysOfKnowing()
    expect(knowing.propositional).toBe(0.25)
    expect(knowing.procedural).toBe(0.25)
    expect(knowing.perspectival).toBe(0.25)
    expect(knowing.participatory).toBe(0.25)
  })
})

describe('Deep Tree Echo - Navigation', () => {
  it('should create navigation with defaults', () => {
    const nav = createNavigation('start', 'goal')
    expect(nav.current).toBe('start')
    expect(nav.destination).toBe('goal')
    expect(nav.path).toEqual(['start'])
    expect(nav.confidence).toBe(0.5)
  })

  it('should update position and add to path', () => {
    const nav = createNavigation('start', 'goal')
    const updated = updateNavigation(nav, 'middle', 0.1)
    expect(updated.current).toBe('middle')
    expect(updated.path).toEqual(['start', 'middle'])
    expect(updated.confidence).toBe(0.6)
  })
})
