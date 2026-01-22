import type { AttributeValue } from '@opentelemetry/api'
import type { ReadableSpan } from '@opentelemetry/sdk-trace-base'
import { InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node'
import { describe, expect, it } from 'vitest'
import { streamText } from '../src'
describe.sequential('streamText', () => {
const memoryExporter = new InMemorySpanExporter()
const tracerProvider = new NodeTracerProvider({
spanProcessors: [new SimpleSpanProcessor(memoryExporter)],
})
tracerProvider.register()
const trimmedAttributes = new Set<string>([
])
const getAttributes = (span: ReadableSpan) => Object.fromEntries(
Object.entries(span.attributes)
.reduce<[string, AttributeValue | undefined][]>((entries, [k, v]) => {
if (k === 'ai.response.text') {
entries.push([k, (v as string).replaceAll('\n', ' ').slice(0, 50)])
}
else if (!trimmedAttributes.has(k)) {
entries.push([k, v])
}
return entries
}, []),
)
it('basic', async () => {
let text = ''
const { textStream } = streamText({
baseURL: 'http://localhost:11434/v1',
messages: [{
content: 'Why is the sky blue?',
role: 'user',
}],
model: 'qwen3:0.6b',
onFinish: async () => {
const spans = memoryExporter.getFinishedSpans().map(getAttributes)
expect(text).toMatchSnapshot()
expect(spans).toMatchSnapshot()
},
seed: 114514,
streamOptions: {
includeUsage: true,
},
telemetry: {
metadata: {
agentId: 'weather-assistant',
instructions: 'You are a helpful weather assistant',
},
},
})
for await (const textDelta of textStream) {
text += textDelta
}
}, 120_000)
})