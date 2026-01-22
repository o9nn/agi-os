import type { Attributes, Span, Tracer } from '@opentelemetry/api'
import { SpanStatusCode } from '@opentelemetry/api'
export const recordErrorOnSpan = (span: Span, error: unknown) => {
if (error instanceof Error) {
span.recordException({
message: error.message,
name: error.name,
stack: error.stack,
})
span.setStatus({
code: SpanStatusCode.ERROR,
message: error.message,
})
}
else {
span.setStatus({ code: SpanStatusCode.ERROR })
}
}
export interface RecordSpanOptions {
attributes: Attributes
endWhenDone?: boolean
name: string
tracer: Tracer
}
export const recordSpan = async <T>({
attributes,
endWhenDone = true,
name,
tracer,
}: RecordSpanOptions, fn: (span: Span) => Promise<T>) =>
tracer.startActiveSpan(name, { attributes }, async (span) => {
try {
const result = await fn(span)
if (endWhenDone)
span.end()
return result
}
catch (error) {
try {
recordErrorOnSpan(span, error)
}
finally {
span.end()
}
throw error
}
})
export const recordSpanSync = <T>({
attributes,
endWhenDone = true,
name,
tracer,
}: RecordSpanOptions, fn: (span: Span) => T) =>
tracer.startActiveSpan(name, { attributes }, (span) => {
try {
const result = fn(span)
if (endWhenDone)
span.end()
return result
}
catch (error) {
try {
recordErrorOnSpan(span, error)
}
finally {
span.end()
}
throw error
}
})