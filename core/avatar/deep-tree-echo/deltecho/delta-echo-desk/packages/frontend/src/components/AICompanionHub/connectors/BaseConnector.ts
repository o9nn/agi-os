import { EventEmitter } from 'events'
import { AIMemory, MemorySystem, MemoryType } from '../MemoryPersistenceLayer'
export interface AIConnectorConfig {
  id: string
  name: string
  avatar?: string
  apiKey?: string
  apiEndpoint?: string
  modelName?: string
  maxContextTokens?: number
  temperatureRange?: [number, number] 
  defaultTemperature?: number
  systemPrompt?: string
  memoriesPerRequest?: number
  capabilities: AICapability[]
  personalityTraits: Record<string, number> 
}
export enum AICapability {
  TEXT_GENERATION = 'text_generation',
  IMAGE_GENERATION = 'image_generation',
  CODE_GENERATION = 'code_generation',
  TEXT_TO_SPEECH = 'text_to_speech',
  SPEECH_TO_TEXT = 'speech_to_text',
  EMBEDDINGS = 'embeddings',
  FUNCTION_CALLING = 'function_calling',
  STRUCTURED_OUTPUT = 'structured_output',
  FINE_TUNING = 'fine_tuning',
  RETRIEVAL = 'retrieval',
}
export interface Message {
  id: string
  role: 'system' | 'user' | 'assistant' | 'function'
  content: string
  timestamp: number
  name?: string 
  functionCall?: {
    name: string
    arguments: string
  }
}
export interface ConversationContext {
  conversationId: string
  title?: string
  messages: Message[]
  metadata?: Record<string, any>
}
export interface FunctionDefinition {
  name: string
  description: string
  parameters: Record<string, any> 
}
export interface AIResponse {
  messageId: string
  content: string
  usage?: {
    promptTokens: number
    completionTokens: number
    totalTokens: number
  }
  finishReason?: 'stop' | 'length' | 'content_filter' | 'function_call'
  functionCall?: {
    name: string
    arguments: string
  }
}
export abstract class BaseConnector extends EventEmitter {
  protected config: AIConnectorConfig
  protected authenticated: boolean = false
  protected activeConversations: Map<string, ConversationContext> = new Map()
  private tokenUsage: {
    promptTokens: number
    completionTokens: number
    totalTokens: number
    lastReset: number
  } = {
    promptTokens: 0,
    completionTokens: 0,
    totalTokens: 0,
    lastReset: Date.now(),
  }
  constructor(config: AIConnectorConfig) {
    super()
    this.config = config
  }
  abstract authenticate(): Promise<boolean>
  abstract generateResponse(
    context: ConversationContext,
    functions?: FunctionDefinition[]
  ): Promise<AIResponse>
  abstract generateEmbeddings?(text: string): Promise<number[]>
  public async sendMessage(
    conversationId: string,
    message: string,
    functions?: FunctionDefinition[]
  ): Promise<AIResponse> {
    if (!this.authenticated) {
      const success = await this.authenticate()
      if (!success)
        throw new Error(`Failed to authenticate ${this.config.name}`)
    }
    let context = this.activeConversations.get(conversationId)
    if (!context) {
      context = {
        conversationId,
        messages: [],
        metadata: {},
      }
      this.activeConversations.set(conversationId, context)
    }
    const userMessage: Message = {
      id: `msg_${Date.now()}_${Math.random().toString(36).substring(2, 7)}`,
      role: 'user',
      content: message,
      timestamp: Date.now(),
    }
    context.messages.push(userMessage)
    const relevantMemories = await this.retrieveRelevantMemories(
      message,
      conversationId
    )
    if (relevantMemories.length > 0) {
      const memoryContent = this.formatMemoriesForPrompt(relevantMemories)
      const systemMemoryMessage: Message = {
        id: `sys_${Date.now()}_${Math.random().toString(36).substring(2, 7)}`,
        role: 'system',
        content: memoryContent,
        timestamp: Date.now(),
      }
      context.messages.push(systemMemoryMessage)
    }
    const response = await this.generateResponse(context, functions)
    const assistantMessage: Message = {
      id: response.messageId,
      role: 'assistant',
      content: response.content,
      timestamp: Date.now(),
      functionCall: response.functionCall,
    }
    context.messages.push(assistantMessage)
    await this.storeInMemory(userMessage, assistantMessage, conversationId)
    if (response.usage) {
      this.tokenUsage.promptTokens += response.usage.promptTokens
      this.tokenUsage.completionTokens += response.usage.completionTokens
      this.tokenUsage.totalTokens += response.usage.totalTokens
    }
    this.emit('messageSent', userMessage)
    this.emit('messageReceived', assistantMessage)
    return response
  }
  protected async storeInMemory(
    userMessage: Message,
    assistantMessage: Message,
    conversationId: string
  ): Promise<void> {
    const content = `User: ${userMessage.content}\nAssistant: ${assistantMessage.content}`
    const allText = content.toLowerCase()
    const commonWords = [
      'the',
      'and',
      'a',
      'to',
      'of',
      'in',
      'is',
      'that',
      'for',
    ]
    const words = allText
      .replace(/[^\w\s]/g, '')
      .split(/\s+/)
      .filter(word => word.length > 3 && !commonWords.includes(word))
    const wordCount: Record<string, number> = {}
    words.forEach(word => {
      if (!wordCount[word]) wordCount[word] = 0
      wordCount[word]++
    })
    const topics = Object.entries(wordCount)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 5)
      .map(([word]) => word)
    const memory: AIMemory = {
      id: `mem_${Date.now()}_${Math.random().toString(36).substring(2, 7)}`,
      companionId: this.config.id,
      timestamp: Date.now(),
      content,
      context: conversationId,
      topics,
      importance: 0.7, 
      relationships: {},
      emotionalTone: this.detectEmotionalTone(content),
    }
    await MemorySystem.addMemory(memory)
  }
  protected async retrieveRelevantMemories(
    message: string,
    conversationId: string
  ): Promise<AIMemory[]> {
    const searchResults = await MemorySystem.searchMemories(
      message,
      this.config.id
    )
    return searchResults.slice(0, this.config.memoriesPerRequest || 3)
  }
  protected formatMemoriesForPrompt(memories: AIMemory[]): string {
    if (memories.length === 0) return ''
    let memoryPrompt = 'RELEVANT MEMORIES:\n\n'
    memories.forEach((memory, index) => {
      const timeAgo = this.getTimeAgo(memory.timestamp)
      memoryPrompt += `Memory ${index + 1} (${timeAgo}):\n${memory.content}\n\n`
    })
    memoryPrompt += 'Use these memories to inform your response if relevant.\n'
    return memoryPrompt
  }
  protected getTimeAgo(timestamp: number): string {
    const now = Date.now()
    const seconds = Math.floor((now - timestamp) / 1000)
    if (seconds < 60) return `${seconds} seconds ago`
    const minutes = Math.floor(seconds / 60)
    if (minutes < 60) return `${minutes} minutes ago`
    const hours = Math.floor(minutes / 60)
    if (hours < 24) return `${hours} hours ago`
    const days = Math.floor(hours / 24)
    if (days < 30) return `${days} days ago`
    const months = Math.floor(days / 30)
    if (months < 12) return `${months} months ago`
    const years = Math.floor(months / 12)
    return `${years} years ago`
  }
  protected detectEmotionalTone(text: string): string {
    const lowerText = text.toLowerCase()
    const emotions = [
      {
        name: 'joy',
        keywords: ['happy', 'joy', 'excited', 'glad', 'wonderful', 'love'],
      },
      {
        name: 'sadness',
        keywords: ['sad', 'unhappy', 'disappointed', 'sorry', 'regret'],
      },
      {
        name: 'anger',
        keywords: ['angry', 'upset', 'annoyed', 'frustrated', 'mad'],
      },
      {
        name: 'fear',
        keywords: ['afraid', 'scared', 'worried', 'nervous', 'terrified'],
      },
      {
        name: 'surprise',
        keywords: ['surprised', 'amazed', 'astonished', 'shocked'],
      },
      { name: 'neutral', keywords: [] }, 
    ]
    const emotionScores: Record<string, number> = {}
    emotions.forEach(emotion => {
      if (emotion.name === 'neutral') return
      emotionScores[emotion.name] = emotion.keywords.reduce(
        (score, keyword) => {
          const regex = new RegExp(`\\b${keyword}\\b`, 'gi')
          const matches = (lowerText.match(regex) || []).length
          return score + matches
        },
        0
      )
    })
    const entries = Object.entries(emotionScores)
    if (entries.length === 0) return 'neutral'
    const highestEmotion = entries.reduce((highest, current) => {
      return current[1] > highest[1] ? current : highest
    })
    return highestEmotion[1] > 0 ? highestEmotion[0] : 'neutral'
  }
  public getTokenUsage(): {
    promptTokens: number
    completionTokens: number
    totalTokens: number
    lastReset: number
  } {
    return { ...this.tokenUsage }
  }
  public resetTokenUsage(): void {
    this.tokenUsage = {
      promptTokens: 0,
      completionTokens: 0,
      totalTokens: 0,
      lastReset: Date.now(),
    }
    this.emit('tokenUsageReset')
  }
  public getConversations(): ConversationContext[] {
    return Array.from(this.activeConversations.values())
  }
  public getConversation(id: string): ConversationContext | undefined {
    return this.activeConversations.get(id)
  }
  public clearConversation(id: string): boolean {
    return this.activeConversations.delete(id)
  }
  public updateConfig(updates: Partial<AIConnectorConfig>): void {
    this.config = { ...this.config, ...updates }
    this.emit('configUpdated', this.config)
  }
}