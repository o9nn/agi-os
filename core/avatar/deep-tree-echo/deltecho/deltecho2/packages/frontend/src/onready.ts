let callbacks: (() => void)[] = []
export function onReady(cb: () => void) {
callbacks.push(cb)
}
export function runPostponedFunctions() {
const todo = [...callbacks]
callbacks = []
todo.forEach(cb => setTimeout(cb, 0))
}