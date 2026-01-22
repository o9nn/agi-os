export async function* readableStreamToAsyncIterator<T>(res: ReadableStream<T>): AsyncGenerator<T, void, unknown> {
const reader = res.getReader()
try {
while (true) {
const { done, value } = await reader.read()
if (done) {
return
}
yield value
}
}
finally {
reader.releaseLock()
}
}