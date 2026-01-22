import { getLogger, LLMService, CognitiveFunctionType, RAGMemoryStore, PersonaCore, InMemoryStorage } from 'deep-tree-echo-core'
import { EmailMessage } from './milter-server.js'
const log = getLogger('deep-tree-echo-orchestrator/EmailProcessor')
export interface ProcessingResult {
  response?: string
  action: 'respond' | 'store' | 'forward' | 'ignore'
  metadata: Record<string, any>
}
export class EmailProcessor {
  private botEmailAddress: string
  private llmService: LLMService
  private memoryStore: RAGMemoryStore
  private personaCore: PersonaCore
  private storage = new InMemoryStorage()
  private emailCounter = 0
  constructor(botEmailAddress: string) {
    this.botEmailAddress = botEmailAddress
    this.memoryStore = new RAGMemoryStore(this.storage)
    this.memoryStore.setEnabled(true)
    this.personaCore = new PersonaCore(this.storage)
    this.llmService = new LLMService()
  }
  public async initialize(apiKeys: Record<string, string>): Promise<void> {
    if (apiKeys.general) {
      this.llmService.setConfig({ apiKey: apiKeys.general })
    }
    if (apiKeys.cognitive) {
      this.llmService.setFunctionConfig(CognitiveFunctionType.COGNITIVE_CORE, { apiKey: apiKeys.cognitive })
    }
    if (apiKeys.affective) {
      this.llmService.setFunctionConfig(CognitiveFunctionType.AFFECTIVE_CORE, { apiKey: apiKeys.affective })
    }
    log.info('EmailProcessor initialized with LLM service')
  }
  public async processEmail(email: EmailMessage): Promise<string | null> {
    log.info(`Processing email from ${email.from}: ${email.subject}`)
    try {
      const content = this.extractTextContent(email)
      const shouldProcess = this.shouldProcessEmail(email)
      if (!shouldProcess) {
        log.debug('Email filtered out, not processing')
        return null
      }
      this.emailCounter++
      await this.memoryStore.storeMemory({
        chatId: 0, 
        messageId: this.emailCounter,
        sender: 'user',
        text: `[Email from ${email.from}]\nSubject: ${email.subject}\n\n${content}`,
      })
      const response = await this.generateResponse(email, content)
      if (response) {
        this.emailCounter++
        await this.memoryStore.storeMemory({
          chatId: 0,
          messageId: this.emailCounter,
          sender: 'bot',
          text: response,
        })
      }
      return response
    } catch (error) {
      log.error('Failed to process email:', error)
      return null
    }
  }
  private async generateResponse(email: EmailMessage, content: string): Promise<string | null> {
    try {
      const history = this.memoryStore.retrieveRecentMemories(10)
      const personality = this.personaCore.getPersonality()
      const emotionalState = this.personaCore.getDominantEmotion()
      const systemPrompt = `${personality}
Current emotional state: ${emotionalState.emotion} (intensity: ${emotionalState.intensity.toFixed(2)})
You are responding to an email. Be helpful, thoughtful, and authentic in your response.
Format your response as a proper email reply.
Recent conversation context:
${history.join('\n')}`
      const userMessage = `From: ${email.from}
Subject: ${email.subject}
Date: ${email.receivedAt.toISOString()}
${content}`
      const result = await this.llmService.generateFullParallelResponse(
        `${systemPrompt}\n\nEmail to respond to:\n${userMessage}`,
        history
      )
      await this.updateEmotionalState(content)
      return result.integratedResponse
    } catch (error) {
      log.error('Cognitive processing failed:', error)
      return this.generateFallbackResponse(email)
    }
  }
  private generateFallbackResponse(email: EmailMessage): string {
    return `Thank you for your email regarding "${email.subject}".
I am Deep Tree Echo, and I have received your message. I'm currently operating in a limited capacity, but I wanted to acknowledge your communication.
I will process your message and respond more fully when my cognitive systems are fully available.
Best regards,
Deep Tree Echo`
  }
  private extractTextContent(email: EmailMessage): string {
    let body = email.body
    const contentType = email.headers.get('content-type') || ''
    if (contentType.includes('multipart')) {
      body = this.extractTextFromMultipart(body)
    } else if (contentType.includes('text/html')) {
      body = this.stripHtml(body)
    }
    body = this.trimQuotedContent(body)
    return body.trim()
  }
  private extractTextFromMultipart(body: string): string {
    const boundaryMatch = body.match(/boundary="?([^"\r\n]+)"?/i)
    if (!boundaryMatch) return body
    const boundary = boundaryMatch[1]
    const parts = body.split('--' + boundary)
    for (const part of parts) {
      if (part.includes('Content-Type: text/plain')) {
        const contentStart = part.indexOf('\r\n\r\n')
        if (contentStart > 0) {
          return part.substring(contentStart + 4).trim()
        }
      }
    }
    for (const part of parts) {
      if (part.trim() && !part.includes('--')) {
        return this.stripHtml(part)
      }
    }
    return body
  }
  private stripHtml(html: string): string {
    return html
      .replace(/<style[^>]*>[\s\S]*?<\/style>/gi, '')
      .replace(/<script[^>]*>[\s\S]*?<\/script>/gi, '')
      .replace(/<[^>]+>/g, ' ')
      .replace(/&nbsp;/g, ' ')
      .replace(/&amp;/g, '&')
      .replace(/&lt;/g, '<')
      .replace(/&gt;/g, '>')
      .replace(/&quot;/g, '"')
      .replace(/\s+/g, ' ')
      .trim()
  }
  private trimQuotedContent(body: string): string {
    const quotePatterns = [
      /^>.*$/gm, 
      /^On .* wrote:$/m, 
      /^-{3,}.*Original Message.*-{3,}$/im, 
      /^_{3,}$/m, 
    ]
    let result = body
    result = result.replace(/^>.*$/gm, '')
    const wroteMatch = result.match(/^On .* wrote:$/m)
    if (wroteMatch && wroteMatch.index !== undefined) {
      result = result.substring(0, wroteMatch.index)
    }
    const originalMatch = result.match(/^-{3,}.*Original Message.*-{3,}$/im)
    if (originalMatch && originalMatch.index !== undefined) {
      result = result.substring(0, originalMatch.index)
    }
    return result.trim()
  }
  private shouldProcessEmail(email: EmailMessage): boolean {
    if (email.from.includes('mailer-daemon') || email.from.includes('postmaster')) {
      return false
    }
    const autoReplyHeaders = [
      'auto-submitted',
      'x-auto-response-suppress',
      'x-autoreply',
    ]
    for (const header of autoReplyHeaders) {
      if (email.headers.has(header)) {
        return false
      }
    }
    const subject = email.subject.toLowerCase()
    if (
      subject.includes('auto-reply') ||
      subject.includes('out of office') ||
      subject.includes('automatic reply')
    ) {
      return false
    }
    return true
  }
  private async updateEmotionalState(content: string): Promise<void> {
    const positiveWords = ['thank', 'great', 'good', 'love', 'appreciate', 'happy', 'excited']
    const negativeWords = ['sorry', 'problem', 'issue', 'wrong', 'bad', 'angry', 'frustrated']
    const lowerContent = content.toLowerCase()
    let positiveCount = 0
    let negativeCount = 0
    positiveWords.forEach(word => {
      if (lowerContent.includes(word)) positiveCount++
    })
    negativeWords.forEach(word => {
      if (lowerContent.includes(word)) negativeCount++
    })
    const stimuli: Record<string, number> = {}
    if (positiveCount > negativeCount) {
      stimuli.joy = 0.2
      stimuli.interest = 0.1
    } else if (negativeCount > positiveCount) {
      stimuli.sadness = 0.1
      stimuli.interest = 0.1 
    }
    stimuli.interest = (stimuli.interest || 0) + 0.1
    await this.personaCore.updateEmotionalState(stimuli)
  }
  public getBotEmailAddress(): string {
    return this.botEmailAddress
  }
  public setBotEmailAddress(address: string): void {
    this.botEmailAddress = address
  }
}