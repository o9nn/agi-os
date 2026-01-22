import type { Message } from '../define/types/mes_example'
const prefixAndSuffix = <T extends string = string>(prefix: string, suffix: string = prefix) =>
(str: string | string[] | TemplateStringsArray, ...substitutions: unknown[]): T =>
`${prefix}${
substitutions.length > 0
? String.raw(str as TemplateStringsArray, substitutions)
: Array.isArray(str)
? str.join(' ')
: str
}${suffix}` as T
export const action = prefixAndSuffix('*')
export const message = prefixAndSuffix('"')
export const char = prefixAndSuffix<Message>('{{char}}: ', '')
export const user = prefixAndSuffix<Message>('{{user}}: ', '')
export {
action as act,
message as msg,
}