import type { Ref } from 'vue'
import { onBeforeUnmount, unref, watch } from 'vue'
export interface UseScrollToHashOptions {
offset?: number
behavior?: ScrollBehavior
maxRetries?: number
retryDelay?: number
scrollContainer?: HTMLElement | string | null
auto?: boolean
}
export function useScrollToHash(
hashRef?: Ref<string | undefined> | (() => string | undefined),
options: UseScrollToHashOptions = {},
) {
const {
offset = 16,
behavior = 'smooth',
maxRetries = 10,
retryDelay = 100,
scrollContainer = null,
auto = false,
} = options
let retryTimer: number | undefined
const getScrollContainer = (): Window | HTMLElement => {
if (!scrollContainer)
return window
if (typeof scrollContainer === 'string') {
const el = document.querySelector(scrollContainer)
return el instanceof HTMLElement ? el : window
}
return scrollContainer
}
const scrollToHash = (hash?: string, attempt = 0) => {
if (!hash)
return
if (retryTimer) {
clearTimeout(retryTimer)
retryTimer = undefined
}
requestAnimationFrame(() => {
const el = hash.length > 1 ? document.getElementById(hash.slice(1)) : null
if (el) {
const container = getScrollContainer()
if (container instanceof Window) {
const top = el.getBoundingClientRect().top + window.scrollY - offset
window.scrollTo({ top, behavior })
}
else {
const containerRect = container.getBoundingClientRect()
const elRect = el.getBoundingClientRect()
const scrollTop = elRect.top - containerRect.top + container.scrollTop - offset
container.scrollTo({ top: scrollTop, behavior })
}
return
}
if (attempt < maxRetries) {
retryTimer = window.setTimeout(() => scrollToHash(hash, attempt + 1), retryDelay)
}
})
}
if (auto && hashRef) {
watch(
() => (typeof hashRef === 'function' ? hashRef() : unref(hashRef)),
(newHash) => {
if (newHash)
scrollToHash(newHash)
},
{ immediate: true },
)
}
onBeforeUnmount(() => {
if (retryTimer)
clearTimeout(retryTimer)
})
return { scrollToHash }
}