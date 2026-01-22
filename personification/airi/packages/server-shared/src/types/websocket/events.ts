export interface DiscordGuildMember {
nickname: string
displayName: string
id: string
}
export interface Discord {
guildMember?: DiscordGuildMember
guildId?: string
channelId?: string
}
interface InputSource {
browser: string
discord: Discord
}
export interface WebSocketBaseEvent<T, D> {
type: T
data: D
}
export type WithInputSource<Source extends keyof InputSource> = {
[S in Source]: InputSource[S]
}
export interface WebSocketEvents<C = undefined> {
'error': {
message: string
}
'module:authenticate': {
token: string
}
'module:authenticated': {
authenticated: boolean
}
'module:announce': {
name: string
possibleEvents: Array<(keyof WebSocketEvents<C>)>
}
'module:configure': {
config: C
}
'ui:configure': {
moduleName: string
moduleIndex?: number
config: C | Record<string, unknown>
}
'input:text': {
text: string
} & Partial<WithInputSource<'browser' | 'discord'>>
'input:text:voice': {
transcription: string
} & Partial<WithInputSource<'browser' | 'discord'>>
'input:voice': {
audio: ArrayBuffer
} & Partial<WithInputSource<'browser' | 'discord'>>
'vscode:context': C
}
export type WebSocketEvent<C = undefined> = {
[K in keyof WebSocketEvents<C>]: WebSocketBaseEvent<K, WebSocketEvents<C>[K]>;
}[keyof WebSocketEvents<C>]