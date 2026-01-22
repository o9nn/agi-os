import type { Entity } from 'prismarine-entity'
interface ChatMessage {
readonly sender: {
username: string
entity: Entity | null
}
readonly content: string
}
export class ChatMessageHandler {
constructor(private readonly botUsername: string) {}
createMessageContext(entity: Entity | null, username: string, content: string): ChatMessage {
return {
sender: {
username,
entity,
},
content,
}
}
isBotMessage(username: string): boolean {
return username === this.botUsername
}
isCommand(content: string): boolean {
return content.startsWith('#')
}
handleChat(callback: (username: string, message: string) => void): (username: string, message: string) => void {
return (username: string, message: string) => {
if (!this.isBotMessage(username)) {
callback(username, message)
}
}
}
}