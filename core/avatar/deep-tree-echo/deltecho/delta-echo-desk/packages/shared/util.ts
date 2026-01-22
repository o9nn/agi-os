export function truncateText(text: string, max_len: number) {
if (text.length > max_len) {
return text.slice(0, max_len) + '…'
} else {
return text
}
}
export function isInviteLink(url: string) {
return url.startsWith('https://i.delta.chat/') && url.includes('#')
}
export function throttle<R, A extends any[]>(
fn: (...args: A) => R,
wait: number
) {
let inThrottle: boolean,
timeout: ReturnType<typeof setTimeout>,
lastTime: number
const ret = (...args: A) => {
if (!inThrottle) {
fn(...args)
lastTime = performance.now()
inThrottle = true
} else {
clearTimeout(timeout)
timeout = setTimeout(
() => {
fn(...args)
lastTime = performance.now()
},
Math.max(wait - (performance.now() - lastTime), 0)
)
}
}
ret.cancel = () => {
clearTimeout(timeout)
}
return ret
}