import {
isProxy,
isReactive,
isRef,
toRaw,
} from 'vue'
export function deepToRaw<T>(input: T): T {
if (Array.isArray(input)) {
return input.map(deepToRaw) as T
}
if (isRef(input) || isReactive(input) || isProxy(input)) {
return deepToRaw(toRaw(input))
}
if (input != null && typeof input === 'object') {
return Object.keys(input).reduce((acc, key) => {
(acc as Record<string, unknown>)[key] = deepToRaw(input[key as keyof T])
return acc
}, {} as T)
}
return input
}