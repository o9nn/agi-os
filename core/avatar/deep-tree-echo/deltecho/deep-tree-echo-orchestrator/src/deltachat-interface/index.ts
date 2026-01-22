import { getLogger } from 'deep-tree-echo-core'
import * as net from 'net'
import { EventEmitter } from 'events'
const log = getLogger('deep-tree-echo-orchestrator/DeltaChatInterface')
class JsonRpcClient {
private requestId: number = 0
private pendingRequests: Map<number, {
resolve: (value: any) => void
reject: (error: Error) => void
}> = new Map()
private sendFn: (msg: string) => void
constructor(sendFn: (msg: string) => void) {
this.sendFn = sendFn
}
call(method: string, params: any[] = []): Promise<any> {
return new Promise((resolve, reject) => {
const id = ++this.requestId
const request = {
jsonrpc: '2.0',
id,
method,
params,
}
this.pendingRequests.set(id, { resolve, reject })
this.sendFn(JSON.stringify(request))
})
}
handleMessage(message: string): void {
try {
const response = JSON.parse(message)
if (response.id !== undefined) {
const pending = this.pendingRequests.get(response.id)
if (pending) {
this.pendingRequests.delete(response.id)
if (response.error) {
pending.reject(new Error(response.error.message || 'RPC Error'))
} else {
pending.resolve(response.result)
}
}
}
} catch (error) {
log.error('Failed to parse JSON-RPC message:', error)
}
}
clearPending(): void {
for (const [, pending] of this.pendingRequests) {
pending.reject(new Error('Connection closed'))
}
this.pendingRequests.clear()
}
}
export interface DeltaChatMessage {
id: number
chatId: number
fromId: number
text: string
timestamp: number
isInfo: boolean
isForwarded: boolean
hasHtml: boolean
viewtype: string
file?: string
fileMime?: string
fileName?: string
subject?: string
overrideSenderName?: string
}
export interface DeltaChatContact {
id: number
address: string
name: string
displayName: string
profileImage?: string
color: number
isBlocked: boolean
isVerified: boolean
}
export interface DeltaChatChat {
id: number
name: string
isProtected: boolean
profileImage?: string
archived: boolean
chatType: 'Single' | 'Group' | 'Mailinglist' | 'Broadcast'
isContactRequest: boolean
isSelfTalk: boolean
isDeviceChat: boolean
selfInGroup: boolean
isUnpromoted: boolean
canSend: boolean
isMuted: boolean
}
export interface DeltaChatAccount {
id: number
email?: string
displayName?: string
profileImage?: string
}
export type DeltaChatEventType =
| 'Info'
| 'SmtpConnected'
| 'ImapConnected'
| 'SmtpMessageSent'
| 'IncomingMsg'
| 'MsgsChanged'
| 'ReactionsChanged'
| 'IncomingMsgBunch'
| 'MsgDelivered'
| 'MsgFailed'
| 'MsgRead'
| 'MsgDeleted'
| 'ChatModified'
| 'ChatEphemeralTimerModified'
| 'ContactsChanged'
| 'LocationChanged'
| 'ConfigureProgress'
| 'ImexProgress'
| 'ImexFileWritten'
| 'SecurejoinInviterProgress'
| 'SecurejoinJoinerProgress'
| 'ConnectivityChanged'
| 'SelfavatarChanged'
| 'ConfigSynced'
| 'WebxdcStatusUpdate'
| 'WebxdcInstanceDeleted'
| 'Error'
| 'ErrorSelfNotInGroup'
| 'Warning'
export interface DeltaChatEvent {
kind: DeltaChatEventType
accountId: number
chatId?: number
msgId?: number
contactId?: number
msg?: string
[key: string]: any
}
export interface DeltaChatConfig {
socketPath?: string
host?: string
port?: number
timeout?: number
autoReconnect?: boolean
reconnectInterval?: number
}
const DEFAULT_CONFIG: DeltaChatConfig = {
socketPath: '/run/deltachat-rpc-server/socket',
timeout: 30000,
autoReconnect: true,
reconnectInterval: 5000,
}
export class DeltaChatInterface extends EventEmitter {
private config: DeltaChatConfig
private client: JsonRpcClient | null = null
private socket: net.Socket | null = null
private connected: boolean = false
private reconnecting: boolean = false
private accountEventListeners: Map<number, boolean> = new Map()
constructor(config: Partial<DeltaChatConfig> = {}) {
super()
this.config = { ...DEFAULT_CONFIG, ...config }
}
public async connect(): Promise<void> {
if (this.connected) {
log.warn('Already connected to DeltaChat')
return
}
log.info('Connecting to DeltaChat RPC server...')
try {
this.socket = await this.createSocket()
this.client = new JsonRpcClient((msg: string) => {
if (this.socket && !this.socket.destroyed) {
this.socket.write(msg + '\n')
}
})
let buffer = ''
this.socket.on('data', (data: Buffer) => {
buffer += data.toString()
const lines = buffer.split('\n')
buffer = lines.pop() || ''
for (const line of lines) {
if (line.trim()) {
try {
this.client?.handleMessage(line)
} catch (error) {
log.error('Error handling RPC message:', error)
}
}
}
})
this.socket.on('close', () => {
this.handleDisconnect()
})
this.socket.on('error', (error) => {
log.error('Socket error:', error)
this.handleDisconnect()
})
this.connected = true
log.info('Connected to DeltaChat RPC server')
this.emit('connected')
await this.startEventListening()
} catch (error) {
log.error('Failed to connect to DeltaChat:', error)
this.handleDisconnect()
throw error
}
}
private createSocket(): Promise<net.Socket> {
return new Promise((resolve, reject) => {
const socket = new net.Socket()
const timeoutId = setTimeout(() => {
socket.destroy()
reject(new Error('Connection timeout'))
}, this.config.timeout)
const connectHandler = () => {
clearTimeout(timeoutId)
resolve(socket)
}
const errorHandler = (error: Error) => {
clearTimeout(timeoutId)
reject(error)
}
socket.once('connect', connectHandler)
socket.once('error', errorHandler)
if (this.config.socketPath) {
log.debug(`Connecting to Unix socket: ${this.config.socketPath}`)
socket.connect(this.config.socketPath)
} else if (this.config.host && this.config.port) {
log.debug(`Connecting to TCP: ${this.config.host}:${this.config.port}`)
socket.connect(this.config.port, this.config.host)
} else {
reject(new Error('No connection method specified'))
}
})
}
private handleDisconnect(): void {
const wasConnected = this.connected
this.connected = false
if (this.client) {
this.client.clearPending()
}
this.socket = null
this.client = null
this.accountEventListeners.clear()
if (wasConnected) {
log.info('Disconnected from DeltaChat')
this.emit('disconnected')
}
if (this.config.autoReconnect && !this.reconnecting) {
this.reconnecting = true
log.info(`Will attempt reconnect in ${this.config.reconnectInterval}ms`)
setTimeout(async () => {
this.reconnecting = false
try {
await this.connect()
} catch (error) {
log.error('Reconnection failed:', error)
}
}, this.config.reconnectInterval)
}
}
private async startEventListening(): Promise<void> {
try {
const accounts = await this.getAllAccounts()
for (const account of accounts) {
await this.startAccountEventListening(account.id)
}
log.info(`Started event listening on ${accounts.length} accounts`)
} catch (error) {
log.error('Failed to start event listening:', error)
}
}
private async startAccountEventListening(accountId: number): Promise<void> {
if (this.accountEventListeners.has(accountId)) {
return
}
this.accountEventListeners.set(accountId, true)
log.debug(`Starting event listener for account ${accountId}`)
this.pollEvents(accountId)
}
private async pollEvents(accountId: number): Promise<void> {
while (this.connected && this.accountEventListeners.has(accountId)) {
try {
const event = await this.call('get_next_event', [accountId]) as DeltaChatEvent | null
if (event) {
this.handleEvent(accountId, event)
}
} catch (error) {
if (this.connected) {
log.error(`Error polling events for account ${accountId}:`, error)
await new Promise(resolve => setTimeout(resolve, 1000))
}
}
}
}
private handleEvent(accountId: number, event: DeltaChatEvent): void {
log.debug(`Event on account ${accountId}: ${event.kind}`)
this.emit('event', { ...event, accountId })
switch (event.kind) {
case 'IncomingMsg':
this.emit('incoming_message', {
accountId,
chatId: event.chatId,
msgId: event.msgId,
})
break
case 'MsgsChanged':
this.emit('messages_changed', {
accountId,
chatId: event.chatId,
msgId: event.msgId,
})
break
case 'ChatModified':
this.emit('chat_modified', {
accountId,
chatId: event.chatId,
})
break
case 'ContactsChanged':
this.emit('contacts_changed', {
accountId,
contactId: event.contactId,
})
break
case 'Error':
case 'Warning':
log.warn(`DeltaChat ${event.kind}: ${event.msg}`)
this.emit('error', {
accountId,
kind: event.kind,
message: event.msg,
})
break
}
}
private async call(method: string, params: any[] = []): Promise<any> {
if (!this.client || !this.connected) {
throw new Error('Not connected to DeltaChat')
}
return this.client.call(method, params)
}
public async disconnect(): Promise<void> {
if (!this.connected) return
log.info('Disconnecting from DeltaChat...')
this.config.autoReconnect = false
this.accountEventListeners.clear()
if (this.socket) {
this.socket.destroy()
}
if (this.client) {
this.client.clearPending()
}
this.connected = false
this.socket = null
this.client = null
log.info('Disconnected from DeltaChat')
}
public isConnected(): boolean {
return this.connected
}
public async getAllAccounts(): Promise<DeltaChatAccount[]> {
const accountIds = await this.call('get_all_account_ids') as number[]
const accounts: DeltaChatAccount[] = []
for (const id of accountIds) {
const info = await this.getAccountInfo(id)
accounts.push({ id, ...info })
}
return accounts
}
public async getAccountInfo(accountId: number): Promise<Partial<DeltaChatAccount>> {
const config = await this.call('get_config', [accountId, 'displayname']) as string | null
const email = await this.call('get_config', [accountId, 'addr']) as string | null
return {
displayName: config || undefined,
email: email || undefined,
}
}
public async selectAccount(accountId: number): Promise<void> {
await this.call('select_account', [accountId])
}
public async getMessage(accountId: number, msgId: number): Promise<DeltaChatMessage> {
return await this.call('get_message', [accountId, msgId])
}
public async sendMessage(accountId: number, chatId: number, text: string): Promise<number> {
const msgId = await this.call('misc_send_text_message', [accountId, chatId, text])
log.info(`Sent message to chat ${chatId}: ${text.substring(0, 50)}...`)
return msgId as number
}
public async sendMessageWithFile(
accountId: number,
chatId: number,
text: string,
filePath?: string,
fileName?: string
): Promise<number> {
const params: any = {
text,
}
if (filePath) {
params.file = filePath
if (fileName) {
params.fileName = fileName
}
}
const msgId = await this.call('send_msg', [accountId, chatId, params])
return msgId as number
}
public async getMessages(
accountId: number,
chatId: number,
flags?: number
): Promise<number[]> {
return await this.call('get_chat_msgs', [accountId, chatId, flags || 0])
}
public async markSeenMessages(accountId: number, msgIds: number[]): Promise<void> {
await this.call('markseen_msgs', [accountId, msgIds])
}
public async getChat(accountId: number, chatId: number): Promise<DeltaChatChat> {
return await this.call('get_chat', [accountId, chatId])
}
public async getAllChats(accountId: number): Promise<DeltaChatChat[]> {
const chatIds = await this.call('get_chatlist_entries', [accountId, null, null, null])
const chats: DeltaChatChat[] = []
for (const entry of chatIds) {
const chatId = typeof entry === 'number' ? entry : entry[0]
const chat = await this.getChat(accountId, chatId)
chats.push(chat)
}
return chats
}
public async createGroupChat(
accountId: number,
name: string,
protect: boolean = false
): Promise<number> {
return await this.call('create_group_chat', [accountId, name, protect])
}
public async createChatByContactId(
accountId: number,
contactId: number
): Promise<number> {
return await this.call('create_chat_by_contact_id', [accountId, contactId])
}
public async addContactToChat(
accountId: number,
chatId: number,
contactId: number
): Promise<void> {
await this.call('add_contact_to_chat', [accountId, chatId, contactId])
}
public async createContact(
accountId: number,
email: string,
name?: string
): Promise<number> {
return await this.call('create_contact', [accountId, email, name || ''])
}
public async getContact(accountId: number, contactId: number): Promise<DeltaChatContact> {
return await this.call('get_contact', [accountId, contactId])
}
public async lookupContactByEmail(
accountId: number,
email: string
): Promise<number | null> {
const contactId = await this.call('lookup_contact_id_by_addr', [accountId, email])
return contactId as number | null
}
public async getSystemInfo(): Promise<Record<string, string>> {
return await this.call('get_system_info', [])
}
public async findOrCreateChatForEmail(
accountId: number,
email: string,
name?: string
): Promise<number> {
let contactId = await this.lookupContactByEmail(accountId, email)
if (!contactId) {
contactId = await this.createContact(accountId, email, name)
}
const chatId = await this.createChatByContactId(accountId, contactId)
return chatId
}
}