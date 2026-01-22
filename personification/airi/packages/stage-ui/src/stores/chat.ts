import type { ChatProvider } from '@xsai-ext/shared-providers'
import type { CommonContentPart, Message, SystemMessage } from '@xsai/shared-chat'
import type { StreamEvent } from '../stores/llm'
import type { ChatAssistantMessage, ChatMessage, ChatSlices } from '../types/chat'
import { useLocalStorage } from '@vueuse/core'
import { defineStore, storeToRefs } from 'pinia'
import { ref, toRaw, watch } from 'vue'
import { useLlmmarkerParser } from '../composables/llmmarkerParser'
import { useLLM } from '../stores/llm'
import { createQueue } from '../utils/queue'
import { TTS_FLUSH_INSTRUCTION } from '../utils/tts'
import { useAiriCardStore } from './modules'
export interface ErrorMessage {
  role: 'error'
  content: string
}
export const useChatStore = defineStore('chat', () => {
  const { stream, discoverToolsCompatibility } = useLLM()
  const { systemPrompt } = storeToRefs(useAiriCardStore())
  const sending = ref(false)
  const onBeforeMessageComposedHooks = ref<Array<(message: string) => Promise<void>>>([])
  const onAfterMessageComposedHooks = ref<Array<(message: string) => Promise<void>>>([])
  const onBeforeSendHooks = ref<Array<(message: string) => Promise<void>>>([])
  const onAfterSendHooks = ref<Array<(message: string) => Promise<void>>>([])
  const onTokenLiteralHooks = ref<Array<(literal: string) => Promise<void>>>([])
  const onTokenSpecialHooks = ref<Array<(special: string) => Promise<void>>>([])
  const onStreamEndHooks = ref<Array<() => Promise<void>>>([])
  const onAssistantResponseEndHooks = ref<Array<(message: string) => Promise<void>>>([])
  function onBeforeMessageComposed(cb: (message: string) => Promise<void>) {
    onBeforeMessageComposedHooks.value.push(cb)
  }
  function onAfterMessageComposed(cb: (message: string) => Promise<void>) {
    onAfterMessageComposedHooks.value.push(cb)
  }
  function onBeforeSend(cb: (message: string) => Promise<void>) {
    onBeforeSendHooks.value.push(cb)
  }
  function onAfterSend(cb: (message: string) => Promise<void>) {
    onAfterSendHooks.value.push(cb)
  }
  function onTokenLiteral(cb: (literal: string) => Promise<void>) {
    onTokenLiteralHooks.value.push(cb)
  }
  function onTokenSpecial(cb: (special: string) => Promise<void>) {
    onTokenSpecialHooks.value.push(cb)
  }
  function onStreamEnd(cb: () => Promise<void>) {
    onStreamEndHooks.value.push(cb)
  }
  function onAssistantResponseEnd(cb: (message: string) => Promise<void>) {
    onAssistantResponseEndHooks.value.push(cb)
  }
  function clearHooks() {
    onBeforeMessageComposedHooks.value = []
    onAfterMessageComposedHooks.value = []
    onBeforeSendHooks.value = []
    onAfterSendHooks.value = []
    onTokenLiteralHooks.value = []
    onTokenSpecialHooks.value = []
    onStreamEndHooks.value = []
    onAssistantResponseEndHooks.value = []
  }
  const codeBlockSystemPrompt = '- For any programming code block, always specify the programming language that supported on @shikijs/rehype on the rendered markdown, eg. ```python ... ```\n'
  const mathSyntaxSystemPrompt = '- For any math equation, use LaTeX format, eg: $ x^3 $, always escape dollar sign outside math equation\n'
  function generateInitialMessage() {
    return {
      role: 'system',
      content: codeBlockSystemPrompt + mathSyntaxSystemPrompt + systemPrompt.value,
    } satisfies SystemMessage
  }
  const messages = useLocalStorage<Array<ChatMessage | ErrorMessage>>('chat/messages', [generateInitialMessage()])
  function cleanupMessages() {
    messages.value = [generateInitialMessage()]
  }
  watch(systemPrompt, () => {
    if (messages.value.length > 0 && messages.value[0].role === 'system') {
      messages.value[0] = generateInitialMessage()
    }
  }, {
    immediate: true,
  })
  const streamingMessage = ref<ChatAssistantMessage>({ role: 'assistant', content: '', slices: [], tool_results: [] })
  async function send(
    sendingMessage: string,
    options: {
      model: string
      chatProvider: ChatProvider
      providerConfig?: Record<string, unknown>
      attachments?: { type: 'image', data: string, mimeType: string }[]
    },
  ) {
    if (!sendingMessage && !options.attachments?.length)
      return
    sending.value = true
    try {
      for (const hook of onBeforeMessageComposedHooks.value) {
        await hook(sendingMessage)
      }
      const contentParts: CommonContentPart[] = [{ type: 'text', text: sendingMessage }]
      if (options.attachments) {
        for (const attachment of options.attachments) {
          if (attachment.type === 'image') {
            contentParts.push({
              type: 'image_url',
              image_url: {
                url: `data:${attachment.mimeType};base64,${attachment.data}`,
              },
            })
          }
        }
      }
      const finalContent = contentParts.length > 1 ? contentParts : sendingMessage
      messages.value.push({ role: 'user', content: finalContent })
      const parser = useLlmmarkerParser({
        onLiteral: async (literal) => {
          for (const hook of onTokenLiteralHooks.value) {
            await hook(literal)
          }
          streamingMessage.value.content += literal
          const lastSlice = streamingMessage.value.slices.at(-1)
          if (lastSlice?.type === 'text') {
            lastSlice.text += literal
            return
          }
          streamingMessage.value.slices.push({
            type: 'text',
            text: literal,
          })
        },
        onSpecial: async (special) => {
          for (const hook of onTokenSpecialHooks.value) {
            await hook(special)
          }
        },
        minLiteralEmitLength: 24, 
      })
      const toolCallQueue = createQueue<ChatSlices>({
        handlers: [
          async (ctx) => {
            if (ctx.data.type === 'tool-call') {
              streamingMessage.value.slices.push(ctx.data)
              return
            }
            if (ctx.data.type === 'tool-call-result') {
              streamingMessage.value.tool_results.push(ctx.data)
            }
          },
        ],
      })
      streamingMessage.value = { role: 'assistant', content: '', slices: [], tool_results: [] }
      const newMessages = messages.value.map((msg) => {
        if (msg.role === 'assistant') {
          const { slices: _, ...rest } = msg 
          rest.tool_results = toRaw(rest.tool_results)
          return toRaw(rest)
        }
        return toRaw(msg)
      })
      for (const hook of onAfterMessageComposedHooks.value) {
        await hook(sendingMessage)
      }
      for (const hook of onBeforeSendHooks.value) {
        await hook(sendingMessage)
      }
      let fullText = ''
      const headers = (options.providerConfig?.headers || {}) as Record<string, string>
      await stream(options.model, options.chatProvider, newMessages as Message[], {
        headers,
        onStreamEvent: async (event: StreamEvent) => {
          switch (event.type) {
            case 'tool-call':
              toolCallQueue.enqueue({
                type: 'tool-call',
                toolCall: event,
              })
              break
            case 'tool-result':
              toolCallQueue.enqueue({
                type: 'tool-call-result',
                id: event.toolCallId,
                result: event.result,
              })
              break
            case 'text-delta':
              fullText += event.text
              await parser.consume(event.text)
              break
            case 'finish':
              break
            case 'error':
              throw event.error ?? new Error('Stream error')
          }
        },
      })
      await parser.end()
      if (streamingMessage.value.slices.length > 0)
        messages.value.push(toRaw(streamingMessage.value))
      streamingMessage.value = { role: 'assistant', content: '', slices: [], tool_results: [] }
      const flushSignal = `${TTS_FLUSH_INSTRUCTION}${TTS_FLUSH_INSTRUCTION}`
      for (const hook of onTokenLiteralHooks.value)
        await hook(flushSignal)
      for (const hook of onStreamEndHooks.value)
        await hook()
      for (const hook of onAssistantResponseEndHooks.value)
        await hook(fullText)
      console.debug('LLM output:', fullText)
      for (const hook of onAfterSendHooks.value) {
        await hook(sendingMessage)
      }
    }
    catch (error) {
      console.error('Error sending message:', error)
      throw error
    }
    finally {
      sending.value = false
    }
  }
  return {
    sending,
    messages,
    streamingMessage,
    discoverToolsCompatibility,
    send,
    cleanupMessages,
    clearHooks,
    onBeforeMessageComposed,
    onAfterMessageComposed,
    onBeforeSend,
    onAfterSend,
    onTokenLiteral,
    onTokenSpecial,
    onStreamEnd,
    onAssistantResponseEnd,
  }
})