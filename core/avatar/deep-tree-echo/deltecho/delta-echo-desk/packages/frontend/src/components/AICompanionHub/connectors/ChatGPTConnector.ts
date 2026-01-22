import {
  BaseConnector,
  AIConnectorConfig,
  AICapability,
  ConversationContext,
  FunctionDefinition,
  AIResponse,
  Message,
} from './BaseConnector'
export interface ChatGPTConfig extends AIConnectorConfig {
  modelName:
    | 'gpt-4o'
    | 'gpt-4-turbo'
    | 'gpt-4'
    | 'gpt-3.5-turbo'
    | 'gpt-4-vision-preview'
    | string
  apiVersion?: string
  organization?: string
  maxTokens?: number
  presencePenalty?: number
  frequencyPenalty?: number
  responseFormat?: {
    type: 'text' | 'json_object'
  }
  visionEnabled?: boolean
}
interface ChatCompletionMessage {
  role: 'system' | 'user' | 'assistant' | 'function'
  content:
    | string
    | null
    | Array<{
        type: 'text' | 'image_url'
        text?: string
        image_url?: {
          url: string
          detail?: 'low' | 'high' | 'auto'
        }
      }>
  name?: string
  function_call?: {
    name: string
    arguments: string
  }
}
interface ChatCompletionFunction {
  name: string
  description: string
  parameters: Record<string, any>
}
interface ChatCompletionRequest {
  model: string
  messages: ChatCompletionMessage[]
  functions?: ChatCompletionFunction[]
  function_call?: 'auto' | 'none' | { name: string }
  max_tokens?: number
  temperature?: number
  top_p?: number
  presence_penalty?: number
  frequency_penalty?: number
  response_format?: {
    type: 'text' | 'json_object'
  }
  stream?: boolean
}
interface ChatCompletionResponse {
  id: string
  object: string
  created: number
  model: string
  choices: Array<{
    index: number
    message: {
      role: string
      content: string | null
      function_call?: {
        name: string
        arguments: string
      }
    }
    finish_reason: 'stop' | 'length' | 'function_call' | 'content_filter'
  }>
  usage: {
    prompt_tokens: number
    completion_tokens: number
    total_tokens: number
  }
}
export class ChatGPTConnector extends BaseConnector {
  private gptConfig: ChatGPTConfig
  constructor(config: ChatGPTConfig) {
    const defaultConfig: Partial<ChatGPTConfig> = {
      apiVersion: '2023-12-01',
      modelName: 'gpt-4o',
      maxTokens: 2048,
      defaultTemperature: 0.7,
      presencePenalty: 0,
      frequencyPenalty: 0,
      visionEnabled: true,
      capabilities: [
        AICapability.TEXT_GENERATION,
        AICapability.CODE_GENERATION,
        AICapability.FUNCTION_CALLING,
        AICapability.STRUCTURED_OUTPUT,
        AICapability.EMBEDDINGS,
      ],
      personalityTraits: {
        creativity: 0.8,
        helpfulness: 0.9,
        precision: 0.85,
        knowledge: 0.9,
        adaptability: 0.95,
      },
    }
    const mergedConfig = { ...defaultConfig, ...config } as ChatGPTConfig
    super(mergedConfig)
    this.gptConfig = mergedConfig
  }
  async authenticate(): Promise<boolean> {
    try {
      if (!this.gptConfig.apiKey) {
        throw new Error('OpenAI API key is required')
      }
      const testResponse = await fetch('https://api.openai.com/v1/models', {
        method: 'GET',
        headers: {
          Authorization: `Bearer ${this.gptConfig.apiKey}`,
          'OpenAI-Organization': this.gptConfig.organization || '',
        },
      })
      if (!testResponse.ok) {
        const errorData = await testResponse.json()
        throw new Error(
          `OpenAI API authentication failed: ${
            errorData.error?.message || testResponse.statusText
          }`
        )
      }
      this.authenticated = true
      this.emit('authenticated')
      return true
    } catch (error) {
      console.error('ChatGPT authentication error:', error)
      this.authenticated = false
      this.emit('authenticationFailed', error)
      return false
    }
  }
  private formatGPTMessages(
    context: ConversationContext
  ): ChatCompletionMessage[] {
    const messages: ChatCompletionMessage[] = []
    const systemMessages = context.messages.filter(msg => msg.role === 'system')
    const nonSystemMessages = context.messages.filter(
      msg => msg.role !== 'system'
    )
    for (const msg of systemMessages) {
      messages.push({
        role: 'system',
        content: msg.content,
      })
    }
    if (systemMessages.length === 0 && this.gptConfig.systemPrompt) {
      messages.push({
        role: 'system',
        content: this.gptConfig.systemPrompt,
      })
    }
    for (const msg of nonSystemMessages) {
      if (msg.role === 'function') {
        messages.push({
          role: 'function',
          name: msg.name || 'unknown_function',
          content: msg.content,
        })
      } else if (msg.functionCall) {
        messages.push({
          role: msg.role,
          content: null, 
          function_call: {
            name: msg.functionCall.name,
            arguments: msg.functionCall.arguments,
          },
        })
      } else {
        messages.push({
          role: msg.role,
          content: msg.content,
        })
      }
    }
    return messages
  }
  async generateResponse(
    context: ConversationContext,
    functions?: FunctionDefinition[]
  ): Promise<AIResponse> {
    try {
      const messages = this.formatGPTMessages(context)
      const requestBody: ChatCompletionRequest = {
        model: this.gptConfig.modelName,
        messages,
        temperature: this.gptConfig.defaultTemperature,
        max_tokens: this.gptConfig.maxTokens,
        presence_penalty: this.gptConfig.presencePenalty,
        frequency_penalty: this.gptConfig.frequencyPenalty,
      }
      if (this.gptConfig.responseFormat) {
        requestBody.response_format = this.gptConfig.responseFormat
      }
      if (functions && functions.length > 0) {
        requestBody.functions = functions.map(fn => ({
          name: fn.name,
          description: fn.description,
          parameters: fn.parameters,
        }))
        requestBody.function_call = 'auto'
      }
      const response = await fetch(
        'https://api.openai.com/v1/chat/completions',
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: `Bearer ${this.gptConfig.apiKey || ''}`,
            'OpenAI-Organization': this.gptConfig.organization || '',
          },
          body: JSON.stringify(requestBody),
        }
      )
      if (!response.ok) {
        const errorData = await response.json()
        throw new Error(
          `OpenAI API error: ${errorData.error?.message || response.statusText}`
        )
      }
      const data = (await response.json()) as ChatCompletionResponse
      const choice = data.choices[0]
      return {
        messageId: `${data.id}_${choice.index}`,
        content: choice.message.content || '',
        usage: {
          promptTokens: data.usage.prompt_tokens,
          completionTokens: data.usage.completion_tokens,
          totalTokens: data.usage.total_tokens,
        },
        finishReason: choice.finish_reason,
        functionCall: choice.message.function_call,
      }
    } catch (error) {
      console.error('ChatGPT response generation error:', error)
      throw error
    }
  }
  async generateEmbeddings(text: string): Promise<number[]> {
    try {
      if (!this.authenticated) {
        await this.authenticate()
      }
      const response = await fetch('https://api.openai.com/v1/embeddings', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          Authorization: `Bearer ${this.gptConfig.apiKey || ''}`,
          'OpenAI-Organization': this.gptConfig.organization || '',
        },
        body: JSON.stringify({
          model: 'text-embedding-3-small',
          input: text,
        }),
      })
      if (!response.ok) {
        const errorData = await response.json()
        throw new Error(
          `OpenAI embeddings error: ${
            errorData.error?.message || response.statusText
          }`
        )
      }
      const data = await response.json()
      return data.data[0].embedding
    } catch (error) {
      console.error('Error generating embeddings:', error)
      throw error
    }
  }
  async processMessageWithImages(
    message: string,
    images: Array<{ data: string; mimeType: string }>
  ): Promise<Message> {
    if (!this.gptConfig.visionEnabled) {
      throw new Error(
        'Vision capabilities are not enabled for this ChatGPT connector'
      )
    }
    const content: Array<{
      type: 'text' | 'image_url'
      text?: string
      image_url?: any
    }> = []
    content.push({
      type: 'text',
      text: message,
    })
    for (const image of images) {
      content.push({
        type: 'image_url',
        image_url: {
          url: `data:${image.mimeType};base64,${image.data}`,
          detail: 'high',
        },
      })
    }
    return {
      id: `msg_img_${Date.now()}_${Math.random().toString(36).substring(2, 7)}`,
      role: 'user',
      content: JSON.stringify(content), 
      timestamp: Date.now(),
    }
  }
  async getAvailableModels(): Promise<string[]> {
    try {
      if (!this.authenticated) {
        await this.authenticate()
      }
      const response = await fetch('https://api.openai.com/v1/models', {
        method: 'GET',
        headers: {
          Authorization: `Bearer ${this.gptConfig.apiKey || ''}`,
          'OpenAI-Organization': this.gptConfig.organization || '',
        },
      })
      if (!response.ok) {
        const errorData = await response.json()
        throw new Error(
          `OpenAI models error: ${
            errorData.error?.message || response.statusText
          }`
        )
      }
      const data = await response.json()
      return data.data
        .filter((model: any) => model.id.includes('gpt'))
        .map((model: any) => model.id)
    } catch (error) {
      console.error('Error fetching available models:', error)
      throw error
    }
  }
}