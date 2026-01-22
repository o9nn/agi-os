import type { ReadableSpan } from '@opentelemetry/sdk-trace-base'
import { InMemorySpanExporter, SimpleSpanProcessor } from '@opentelemetry/sdk-trace-base'
import { NodeTracerProvider } from '@opentelemetry/sdk-trace-node'
import { describe, expect, it } from 'vitest'
import { tool } from 'xsai'
import { z } from 'zod/v4'
import { generateText } from '../src'
describe.sequential('generateText with tools', () => {
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
const { text } = await generateText({
baseURL: 'http://localhost:11434/v1',
messages: [{
content: 'How many times does 114514 plus 1919810 equal? Please try to call the `add` tool to solve the problem.',
role: 'user',
}],
model: 'qwen3:0.6b',
seed: 114514,
tools: [add],
})
const spans = memoryExporter.getFinishedSpans().map(getAttributes)
expect(text).toMatchSnapshot()
expect(spans).toMatchSnapshot()
}, 120_000)
})