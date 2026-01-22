import type { AttributeValue } from '@opentelemetry/api'
import type { ReadableSpan } from '@opentelemetry/sdk-trace-base'
import { InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node'
import { describe, expect, it } from 'vitest'
import { tool } from 'xsai'
import { z } from 'zod/v4'
import { streamText } from '../src'
describe.sequential('streamText with tools', () => {
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
const add = await tool({
description: 'Adds two numbers',
execute: ({ a, b }) => (Number.parseInt(a) + Number.parseInt(b)).toString(),
name: 'add',
parameters: z.object({
a: z.string()
.describe('First number'),
b: z.string()
.describe('Second number'),
}),
})
let text = ''
const { textStream } = streamText({
baseURL: 'http://localhost:11434/v1',
maxSteps: 5,
messages: [{
content: 'How many times does 114514 plus 1919810 equal? Please try to call the `add` tool to solve the problem.',
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
tools: [add],
})
for await (const textDelta of textStream) {
text += textDelta
}
}, 120_000)
})