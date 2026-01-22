import { getLogger } from '../utils/logger'
import { MemoryStorage, InMemoryStorage } from './storage'
const log = getLogger('deep-tree-echo-core/memory/RAGMemoryStore')
const DEFAULT_MEMORY_LIMIT = 1000
const DEFAULT_REFLECTION_LIMIT = 100
export interface Memory {
  id: string
  timestamp: number
  chatId: number
  messageId: number
  sender: 'user' | 'bot'
  text: string
  embedding?: number[] 
}
export interface ReflectionMemory {
  id: string
  timestamp: number
  content: string
  type: 'periodic' | 'focused'
  aspect?: string 
}
export class RAGMemoryStore {
  private memories: Memory[] = []
  private reflections: ReflectionMemory[] = []
  private enabled: boolean = false
  private storage: MemoryStorage
  private memoryLimit: number
  private reflectionLimit: number
  constructor(
    storage?: MemoryStorage,
    options?: { memoryLimit?: number; reflectionLimit?: number }
  ) {
    this.storage = storage || new InMemoryStorage()
    this.memoryLimit = options?.memoryLimit || DEFAULT_MEMORY_LIMIT
    this.reflectionLimit = options?.reflectionLimit || DEFAULT_REFLECTION_LIMIT
    this.loadMemories()
  }
  public setEnabled(enabled: boolean): void {
    this.enabled = enabled
    log.info(`Memory system ${enabled ? 'enabled' : 'disabled'}`)
  }
  public isEnabled(): boolean {
    return this.enabled
  }
  private async loadMemories(): Promise<void> {
    try {
      const memoriesData = await this.storage.load('deepTreeEchoBotMemories')
      if (memoriesData) {
        try {
          this.memories = JSON.parse(memoriesData)
          log.info(`Loaded ${this.memories.length} conversation memories`)
        } catch (error) {
          log.error('Failed to parse conversation memories:', error)
          this.memories = []
        }
      }
      const reflectionsData = await this.storage.load(
        'deepTreeEchoBotReflections'
      )
      if (reflectionsData) {
        try {
          this.reflections = JSON.parse(reflectionsData)
          log.info(`Loaded ${this.reflections.length} reflection memories`)
        } catch (error) {
          log.error('Failed to parse reflection memories:', error)
          this.reflections = []
        }
      }
      const enabledData = await this.storage.load('deepTreeEchoBotMemoryEnabled')
      this.enabled = enabledData === 'true'
    } catch (error) {
      log.error('Failed to load memories:', error)
      this.memories = []
      this.reflections = []
    }
  }
  private async saveMemories(): Promise<void> {
    try {
      const trimmedMemories = this.memories.slice(-this.memoryLimit)
      await this.storage.save(
        'deepTreeEchoBotMemories',
        JSON.stringify(trimmedMemories)
      )
      const trimmedReflections = this.reflections.slice(-this.reflectionLimit)
      await this.storage.save(
        'deepTreeEchoBotReflections',
        JSON.stringify(trimmedReflections)
      )
      log.info('Saved memories to persistent storage')
    } catch (error) {
      log.error('Failed to save memories:', error)
    }
  }
  public async storeMemory(
    memory: Omit<Memory, 'id' | 'timestamp' | 'embedding'>
  ): Promise<void> {
    if (!this.enabled) return
    try {
      const newMemory: Memory = {
        ...memory,
        id: `mem_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`,
        timestamp: Date.now(),
        embedding: [], 
      }
      this.memories.push(newMemory)
      await this.saveMemories()
      log.info(`Stored new memory: ${newMemory.id}`)
    } catch (error) {
      log.error('Failed to store memory:', error)
    }
  }
  public async storeReflection(
    content: string,
    type: 'periodic' | 'focused' = 'periodic',
    aspect?: string
  ): Promise<void> {
    if (!this.enabled) return
    try {
      const reflection: ReflectionMemory = {
        id: `ref_${Date.now()}_${Math.random().toString(36).substring(2, 9)}`,
        timestamp: Date.now(),
        content,
        type,
        aspect,
      }
      this.reflections.push(reflection)
      await this.saveMemories()
      log.info(`Stored new ${type} reflection${aspect ? ` on ${aspect}` : ''}`)
    } catch (error) {
      log.error('Failed to store reflection:', error)
    }
  }
  public getMemoriesByChat(chatId: number): Memory[] {
    return this.memories
      .filter(mem => mem.chatId === chatId)
      .sort((a, b) => a.timestamp - b.timestamp)
  }
  public retrieveRecentMemories(count: number = 10): string[] {
    return this.memories
      .sort((a, b) => b.timestamp - a.timestamp)
      .slice(0, count)
      .map(
        mem =>
          `[${new Date(mem.timestamp).toLocaleString()}] ${mem.sender}: ${
            mem.text
          }`
      )
  }
  public getRecentReflections(count: number = 5): ReflectionMemory[] {
    return this.reflections
      .sort((a, b) => b.timestamp - a.timestamp)
      .slice(0, count)
  }
  public async clearAllMemories(): Promise<void> {
    this.memories = []
    await this.saveMemories()
    log.info('Cleared all conversation memories')
  }
  public async clearChatMemories(chatId: number): Promise<void> {
    this.memories = this.memories.filter(mem => mem.chatId !== chatId)
    await this.saveMemories()
    log.info(`Cleared memories for chat ${chatId}`)
  }
  public searchMemories(query: string, limit: number = 5): Memory[] {
    const normalizedQuery = query.toLowerCase()
    return this.memories
      .filter(mem => mem.text.toLowerCase().includes(normalizedQuery))
      .sort((a, b) => b.timestamp - a.timestamp)
      .slice(0, limit)
  }
  public getConversationContext(
    chatId: number,
    messageLimit: number = 10
  ): Memory[] {
    return this.memories
      .filter(mem => mem.chatId === chatId)
      .sort((a, b) => b.timestamp - a.timestamp)
      .slice(0, messageLimit)
      .sort((a, b) => a.timestamp - b.timestamp)
  }
}