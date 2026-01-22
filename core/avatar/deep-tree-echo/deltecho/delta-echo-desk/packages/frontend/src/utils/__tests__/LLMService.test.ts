import { LLMService, ChatMessage } from '../LLMService'
global.fetch = jest.fn()
describe('LLMService', () => {
let llmService: LLMService
beforeEach(() => {
jest.clearAllMocks()
llmService = LLMService.getInstance()
llmService.setConfig({
apiKey: 'test-api-key',
apiEndpoint: 'https://test-api-endpoint.com/v1/chat/completions',
model: 'test-model',
temperature: 0.5,
maxTokens: 500,
})
})
describe('generateResponse', () => {
it('should call the API with correct parameters', async () => {
const mockResponse = {
id: 'mock-response-id',
object: 'chat.completion',
created: Date.now(),
choices: [
{
message: {
role: 'assistant',
content: 'This is a test response',
},
},
],
}
;(global.fetch as jest.Mock).mockResolvedValueOnce({
ok: true,
json: async () => mockResponse,
})
const messages: ChatMessage[] = [
{ role: 'system', content: 'You are a helpful assistant' },
{ role: 'user', content: 'Hello, world!' },
]
const response = await llmService.generateResponse(messages)
expect(global.fetch).toHaveBeenCalledWith(
'https://test-api-endpoint.com/v1/chat/completions',
{
method: 'POST',
headers: {
'Content-Type': 'application/json',
Authorization: 'Bearer test-api-key',
},
body: JSON.stringify({
model: 'test-model',
messages,
temperature: 0.5,
max_tokens: 500,
}),
}
)
expect(response).toBe('This is a test response')
})
it('should throw an error when API response is not ok', async () => {
;(global.fetch as jest.Mock).mockResolvedValueOnce({
ok: false,
status: 400,
json: async () => ({ error: 'Bad request' }),
})
const messages: ChatMessage[] = [
{ role: 'user', content: 'Hello, world!' },
]
await expect(llmService.generateResponse(messages)).rejects.toThrow(
'API Error'
)
})
it('should throw an error when API key is not configured', async () => {
llmService.setConfig({ apiKey: '' })
const messages: ChatMessage[] = [
{ role: 'user', content: 'Hello, world!' },
]
await expect(llmService.generateResponse(messages)).rejects.toThrow(
'API Key is not configured'
)
})
})
describe('generateResponseWithContext', () => {
it('should format messages correctly with conversation history', async () => {
const generateResponseSpy = jest
.spyOn(llmService, 'generateResponse')
.mockResolvedValueOnce('Mocked response')
const userInput = 'What do you think about that?'
const conversationHistory =
'User: Hello\nAssistant: Hi there!\nUser: I have a question'
const systemPrompt = 'You are a helpful assistant'
await llmService.generateResponseWithContext(
userInput,
conversationHistory,
systemPrompt
)
const passedMessages = generateResponseSpy.mock.calls[0][0]
expect(passedMessages).toHaveLength(4)
expect(passedMessages[0]).toEqual({
role: 'system',
content: systemPrompt,
})
expect(passedMessages[1].role).toBe('user')
expect(passedMessages[1].content).toContain(conversationHistory)
expect(passedMessages[2].role).toBe('assistant')
expect(passedMessages[3]).toEqual({ role: 'user', content: userInput })
})
it('should handle empty conversation history', async () => {
const generateResponseSpy = jest
.spyOn(llmService, 'generateResponse')
.mockResolvedValueOnce('Mocked response')
const userInput = 'Hello, who are you?'
const emptyHistory = ''
const systemPrompt = 'You are a helpful assistant'
await llmService.generateResponseWithContext(
userInput,
emptyHistory,
systemPrompt
)
const passedMessages = generateResponseSpy.mock.calls[0][0]
expect(passedMessages).toHaveLength(2)
expect(passedMessages[0]).toEqual({
role: 'system',
content: systemPrompt,
})
expect(passedMessages[1]).toEqual({ role: 'user', content: userInput })
})
})
})