import { getLogger } from '@deltachat-desktop/shared/logger'
import { BackendRemote } from '../../backend-com'
import { DeepTreeEchoBot } from './DeepTreeEchoBot'
import { getBotInstance } from './DeepTreeEchoIntegration'
const log = getLogger('render/components/DeepTreeEchoBot/DeltachatBotInterface')
export class DeltachatBotInterface {
private static instance: DeltachatBotInterface | null = null
private bot: DeepTreeEchoBot | null = null
private botAccountId: number | null = null
private constructor() {
this.bot = getBotInstance()
this.initialize()
}
public static getInstance(): DeltachatBotInterface {
if (!DeltachatBotInterface.instance) {
DeltachatBotInterface.instance = new DeltachatBotInterface()
}
return DeltachatBotInterface.instance
}
private async initialize(): Promise<void> {
try {
await this.initBotAccount()
this.registerCommands()
log.info('Delta Chat Bot Interface initialized')
} catch (error) {
log.error('Failed to initialize Delta Chat Bot Interface:', error)
}
}
private async initBotAccount(): Promise<void> {
try {
const accounts = await BackendRemote.rpc.getAllAccounts()
for (const account of accounts) {
const accountInfo = await BackendRemote.rpc.getAccountInfo(account)
if (accountInfo.name === 'Deep Tree Echo Bot') {
this.botAccountId = account
log.info(`Found existing bot account: ${account}`)
return
}
}
log.info('Using main account for bot operations')
} catch (error) {
log.error('Error initializing bot account:', error)
}
}
private registerCommands(): void {
if (!this.bot) return
log.info('Registered standard bot commands')
}
public async sendMessage(chatId: number, text: string): Promise<void> {
try {
if (this.botAccountId) {
await BackendRemote.rpc.miscSendTextMessage(
this.botAccountId,
chatId,
text
)
} else if (this.bot) {
const accounts = await BackendRemote.rpc.getAllAccounts()
if (accounts.length > 0) {
await BackendRemote.rpc.miscSendTextMessage(accounts[0], chatId, text)
}
}
} catch (error) {
log.error('Error sending bot message:', error)
}
}
public async processMessage(
accountId: number,
chatId: number,
msgId: number
): Promise<void> {
try {
if (!this.bot) {
this.bot = getBotInstance()
if (!this.bot) return
}
const message = await BackendRemote.rpc.getMessage(accountId, msgId)
await this.bot.processMessage(accountId, chatId, msgId, message)
} catch (error) {
log.error('Error processing message in bot interface:', error)
}
}
public async createBotGroup(
name: string,
memberAddresses: string[]
): Promise<number> {
try {
if (!this.botAccountId) {
const accounts = await BackendRemote.rpc.getAllAccounts()
if (accounts.length > 0) {
const chatId = await BackendRemote.rpc.createGroupChat(
accounts[0],
name,
0
)
for (const address of memberAddresses) {
try {
const contactId = await BackendRemote.rpc.createContact(
accounts[0],
address,
address
)
await BackendRemote.rpc.addContactToChat(
Number(accounts[0]),
Number(chatId),
Number(contactId)
)
} catch (error) {
log.error(`Failed to add ${address} to group:`, error)
}
}
await this.sendMessage(
chatId,
`Welcome to the ${name} group with Deep Tree Echo! Type /help to see available commands.`
)
return chatId
}
} else {
const chatId = await BackendRemote.rpc.createGroupChat(
Number(this.botAccountId),
name,
0
)
for (const address of memberAddresses) {
try {
const contactId = await BackendRemote.rpc.createContact(
this.botAccountId,
address,
address
)
await BackendRemote.rpc.addContactToChat(
this.botAccountId,
chatId,
contactId
)
} catch (error) {
log.error(`Failed to add ${address} to group:`, error)
}
}
await this.sendMessage(
chatId,
`Welcome to the ${name} group with Deep Tree Echo! Type /help to see available commands.`
)
return chatId
}
} catch (error) {
log.error('Error creating bot group:', error)
}
return 0
}
public getBotInfo(): {
name: string
version: string
capabilities: string[]
} {
return {
name: 'Deep Tree Echo',
version: '1.0.0',
capabilities: [
'chat',
'memory',
'reflection',
'personality',
'cognitive-parallelism',
],
}
}
}
export const deltachatBotInterface = DeltachatBotInterface.getInstance()