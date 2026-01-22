import { EventEmitter } from 'events'
import { runtime } from '@deltachat-desktop/runtime-interface'
export interface AIMemory {
  id: string
  companionId: string
  timestamp: number
  content: string
  context: string
  emotionalTone?: string
  topics: string[]
  importance: number 
  relationships: Record<string, number> 
  embeddings?: number[] 
}
export enum MemoryType {
  EPISODIC = 'episodic', 
  SEMANTIC = 'semantic', 
  PROCEDURAL = 'procedural', 
  EMOTIONAL = 'emotional', 
  ASSOCIATIVE = 'associative', 
}
export enum ConsolidationStage {
  SHORT_TERM = 'short_term',
  PROCESSING = 'processing',
  LONG_TERM = 'long_term',
  CORE_IDENTITY = 'core_identity',
}
export class MemoryPersistenceLayer extends EventEmitter {
  private static instance: MemoryPersistenceLayer
  private memories: Map<string, AIMemory> = new Map()
  private companions: Set<string> = new Set()
  private initialized: boolean = false
  private consolidationIntervalId: NodeJS.Timeout | null = null
  private constructor() {
    super()
  }
  public static getInstance(): MemoryPersistenceLayer {
    if (!MemoryPersistenceLayer.instance) {
      MemoryPersistenceLayer.instance = new MemoryPersistenceLayer()
    }
    return MemoryPersistenceLayer.instance
  }
  public async initialize(): Promise<void> {
    if (this.initialized) return
    try {
      const settings = await runtime.getDesktopSettings()
      const storedMemories = settings.aiMemories || []
      storedMemories.forEach(memory => {
        this.memories.set(memory.id, memory)
        this.companions.add(memory.companionId)
      })
      this.startMemoryConsolidation()
      this.initialized = true
      this.emit('initialized', { memoryCount: this.memories.size })
      console.log(
        `Memory Persistence Layer initialized with ${this.memories.size} memories across ${this.companions.size} companions`
      )
    } catch (error) {
      console.error('Failed to initialize Memory Persistence Layer:', error)
      throw new Error('Memory system initialization failed')
    }
  }
  public async addMemory(memory: AIMemory): Promise<string> {
    if (!this.initialized) await this.initialize()
    if (!memory.id)
      memory.id = `mem_${Date.now()}_${Math.random()
        .toString(36)
        .substring(2, 9)}`
    if (!memory.timestamp) memory.timestamp = Date.now()
    this.memories.set(memory.id, memory)
    this.companions.add(memory.companionId)
    await this.persistMemories()
    this.emit('memoryAdded', memory)
    return memory.id
  }
  public async updateMemory(
    id: string,
    updates: Partial<AIMemory>
  ): Promise<AIMemory | null> {
    if (!this.initialized) await this.initialize()
    const memory = this.memories.get(id)
    if (!memory) return null
    const updatedMemory = { ...memory, ...updates, id }
    this.memories.set(id, updatedMemory)
    await this.persistMemories()
    this.emit('memoryUpdated', updatedMemory)
    return updatedMemory
  }
  public async deleteMemory(id: string): Promise<boolean> {
    if (!this.initialized) await this.initialize()
    const deleted = this.memories.delete(id)
    if (deleted) {
      await this.persistMemories()
      this.emit('memoryDeleted', id)
    }
    return deleted
  }
  public async getMemoriesByCompanion(
    companionId: string
  ): Promise<AIMemory[]> {
    if (!this.initialized) await this.initialize()
    return Array.from(this.memories.values())
      .filter(memory => memory.companionId === companionId)
      .sort((a, b) => b.timestamp - a.timestamp) 
  }
  public async searchMemories(
    query: string,
    companionId?: string
  ): Promise<AIMemory[]> {
    if (!this.initialized) await this.initialize()
    const normalizedQuery = query.toLowerCase()
    const results = Array.from(this.memories.values())
      .filter(memory => {
        if (companionId && memory.companionId !== companionId) return false
        return (
          memory.content.toLowerCase().includes(normalizedQuery) ||
          memory.context.toLowerCase().includes(normalizedQuery) ||
          memory.topics.some(topic =>
            topic.toLowerCase().includes(normalizedQuery)
          )
        )
      })
      .sort((a, b) => {
        const importanceDiff = b.importance - a.importance
        if (importanceDiff !== 0) return importanceDiff
        return b.timestamp - a.timestamp
      })
    return results
  }
  public async findRelatedMemories(
    memoryId: string,
    limit: number = 5
  ): Promise<AIMemory[]> {
    if (!this.initialized) await this.initialize()
    const memory = this.memories.get(memoryId)
    if (!memory) return []
    const explicitlyRelated = Object.entries(memory.relationships || {})
      .sort(([, strengthA], [, strengthB]) => strengthB - strengthA)
      .slice(0, limit)
      .map(([relatedId]) => this.memories.get(relatedId))
      .filter(Boolean) as AIMemory[]
    if (explicitlyRelated.length >= limit) return explicitlyRelated
    const implicitlyRelated = Array.from(this.memories.values())
      .filter(
        m =>
          m.id !== memoryId &&
          m.companionId === memory.companionId &&
          !memory.relationships[m.id] && 
          m.topics.some(topic => memory.topics.includes(topic))
      )
      .sort((a, b) => {
        const sharedTopicsA = a.topics.filter(topic =>
          memory.topics.includes(topic)
        ).length
        const sharedTopicsB = b.topics.filter(topic =>
          memory.topics.includes(topic)
        ).length
        return sharedTopicsB - sharedTopicsA
      })
      .slice(0, limit - explicitlyRelated.length)
    return [...explicitlyRelated, ...implicitlyRelated]
  }
  private startMemoryConsolidation(): void {
    this.consolidationIntervalId = setInterval(
      () => this.consolidateMemories(),
      60 * 60 * 1000
    )
  }
  private async consolidateMemories(): Promise<void> {
    console.log('Memory consolidation process starting...')
    const companionMemories: Record<string, AIMemory[]> = {}
    this.companions.forEach(companionId => {
      companionMemories[companionId] = Array.from(
        this.memories.values()
      ).filter(memory => memory.companionId === companionId)
    })
    for (const companionId of this.companions) {
      const memories = companionMemories[companionId]
      if (memories.length < 2) continue
      const topicIndex: Record<string, string[]> = {}
      memories.forEach(memory => {
        memory.topics.forEach(topic => {
          if (!topicIndex[topic]) topicIndex[topic] = []
          topicIndex[topic].push(memory.id)
        })
      })
      for (const memory of memories) {
        const relatedMemoryIds = new Set<string>()
        memory.topics.forEach(topic => {
          topicIndex[topic]?.forEach(id => {
            if (id !== memory.id) relatedMemoryIds.add(id)
          })
        })
        relatedMemoryIds.forEach(relatedId => {
          const relatedMemory = this.memories.get(relatedId)
          if (!relatedMemory) return
          const sharedTopics = memory.topics.filter(topic =>
            relatedMemory.topics.includes(topic)
          ).length
          const timeProximity =
            1 /
            (1 +
              Math.abs(memory.timestamp - relatedMemory.timestamp) /
                (24 * 60 * 60 * 1000))
          const strength =
            (0.7 * sharedTopics) /
              Math.max(memory.topics.length, relatedMemory.topics.length) +
            0.3 * timeProximity
          if (!memory.relationships) memory.relationships = {}
          if (!relatedMemory.relationships) relatedMemory.relationships = {}
          memory.relationships[relatedId] = strength
          relatedMemory.relationships[memory.id] = strength
        })
      }
    }
    await this.persistMemories()
    console.log('Memory consolidation process completed')
    this.emit('memoryConsolidation', { timestamp: Date.now() })
  }
  private async persistMemories(): Promise<void> {
    try {
      const settings = await runtime.getDesktopSettings()
      const memoriesArray = Array.from(this.memories.values())
      const updatedSettings = {
        ...settings,
        aiMemories: memoriesArray,
      }
      await runtime.setDesktopSettings(updatedSettings)
      this.emit('memoriesPersisted', { count: memoriesArray.length })
    } catch (error) {
      console.error('Failed to persist memories:', error)
      this.emit('error', { message: 'Failed to persist memories', error })
    }
  }
  public async shutdown(): Promise<void> {
    if (this.consolidationIntervalId) {
      clearInterval(this.consolidationIntervalId)
      this.consolidationIntervalId = null
    }
    await this.consolidateMemories()
    this.initialized = false
    this.emit('shutdown')
  }
}
export const MemorySystem = MemoryPersistenceLayer.getInstance()