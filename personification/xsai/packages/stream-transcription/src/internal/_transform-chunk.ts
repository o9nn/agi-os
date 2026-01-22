import type { StreamTranscriptionDelta } from '..'
const parseChunk = (text: string): [StreamTranscriptionDelta | undefined, boolean] => {
if (!text || !text.startsWith('data:'))
return [undefined, false]
const content = text.slice('data:'.length)
const data = content.startsWith(' ') ? content.slice(1) : content
if (data.includes('[DONE]')) {
return [undefined, true]
}
if (data.startsWith('{') && data.includes('"error":')) {
throw new Error(`Error from server: ${data}`)
}
const chunk = JSON.parse(data) as StreamTranscriptionDelta
return [chunk, false]
}
export const transformChunk = () => {
const decoder = new TextDecoder()
let buffer = ''
return new TransformStream<Uint8Array, StreamTranscriptionDelta>({
transform: async (chunk, controller) => {
const text = decoder.decode(chunk, { stream: true })
buffer += text
const lines = buffer.split('\n')
buffer = lines.pop() ?? ''
for (const line of lines) {
try {
const [chunk, isEnd] = parseChunk(line)
if (isEnd)
break
if (chunk) {
controller.enqueue(chunk)
}
}
catch (error) {
controller.error(error)
}
}
},
})
}