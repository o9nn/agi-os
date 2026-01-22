import type { ReadableSpan } from '@opentelemetry/sdk-trace-base'
import { InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node'
import { describe, expect, it } from 'vitest'
import { generateText } from '../src'
describe.sequential('generateText', () => {
const memoryExporter = new InMemorySpanExporter()
const tracerProvider = new NodeTracerProvider({
spanProcessors: [new SimpleSpanProcessor(memoryExporter)],
})
tracerProvider.register()
const getAttributes = (span: ReadableSpan) => Object.fromEntries(
Object.entries(span.attributes)
.map(([k, v]) => k === 'ai.response.text'
? [k, (v as string).replaceAll('\n', ' ').slice(0, 50)]
: [k, v],
),
)
it('basic', async () => {
const { text } = await generateText({
baseURL: 'http://localhost:11434/v1',
messages: [{
content: 'Why is the sky blue?',
role: 'user',
}],
model: 'qwen3:0.6b',
seed: 114514,
telemetry: {
metadata: {
agentId: 'weather-assistant',
instructions: 'You are a helpful weather assistant',
},
},
})
const spans = memoryExporter.getFinishedSpans().map(getAttributes)
expect(text).toMatchSnapshot()
expect(spans).toMatchSnapshot()
}, 120_000)
})