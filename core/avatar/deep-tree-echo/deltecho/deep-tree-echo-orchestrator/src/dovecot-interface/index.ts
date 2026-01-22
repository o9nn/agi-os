import { getLogger } from 'deep-tree-echo-core'
import { MilterServer, MilterConfig, EmailMessage } from './milter-server.js'
import { LMTPServer, LMTPConfig } from './lmtp-server.js'
import { EmailProcessor } from './email-processor.js'
const log = getLogger('deep-tree-echo-orchestrator/DovecotInterface')
export interface DovecotConfig {
enableMilter: boolean
milterSocket: string
enableLMTP: boolean
lmtpSocket: string
allowedDomains: string[]
botEmailAddress: string
}
const DEFAULT_CONFIG: DovecotConfig = {
enableMilter: true,
milterSocket: '/var/run/deep-tree-echo/milter.sock',
enableLMTP: false,
lmtpSocket: '/var/run/deep-tree-echo/lmtp.sock',
allowedDomains: ['*'],
botEmailAddress: 'echo@localhost',
}
export class DovecotInterface {
private config: DovecotConfig
private milterServer?: MilterServer
private lmtpServer?: LMTPServer
private emailProcessor: EmailProcessor
private running: boolean = false
constructor(config: Partial<DovecotConfig> = {}) {
this.config = { ...DEFAULT_CONFIG, ...config }
this.emailProcessor = new EmailProcessor(this.config.botEmailAddress)
}
public async start(): Promise<void> {
if (this.running) {
log.warn('DovecotInterface is already running')
return
}
log.info('Starting Dovecot integration...')
try {
if (this.config.enableMilter) {
const milterConfig: MilterConfig = {
socketPath: this.config.milterSocket,
allowedDomains: this.config.allowedDomains,
}
this.milterServer = new MilterServer(milterConfig)
this.milterServer.on('email', this.handleIncomingEmail.bind(this))
await this.milterServer.start()
log.info(`Milter server started on ${this.config.milterSocket}`)
}
if (this.config.enableLMTP) {
const lmtpConfig: LMTPConfig = {
socketPath: this.config.lmtpSocket,
allowedDomains: this.config.allowedDomains,
}
this.lmtpServer = new LMTPServer(lmtpConfig)
this.lmtpServer.on('email', this.handleIncomingEmail.bind(this))
await this.lmtpServer.start()
log.info(`LMTP server started on ${this.config.lmtpSocket}`)
}
this.running = true
log.info('Dovecot integration started successfully')
} catch (error) {
log.error('Failed to start Dovecot integration:', error)
await this.stop()
throw error
}
}
public async stop(): Promise<void> {
if (!this.running) return
log.info('Stopping Dovecot integration...')
if (this.milterServer) {
await this.milterServer.stop()
}
if (this.lmtpServer) {
await this.lmtpServer.stop()
}
this.running = false
log.info('Dovecot integration stopped')
}
public isRunning(): boolean {
return this.running
}
private async handleIncomingEmail(email: EmailMessage): Promise<void> {
log.info(`Processing email from ${email.from} to ${email.to.join(', ')}`)
try {
const isForBot = email.to.some(
addr => addr.toLowerCase() === this.config.botEmailAddress.toLowerCase()
)
if (!isForBot) {
log.debug('Email not addressed to Deep Tree Echo, skipping')
return
}
const response = await this.emailProcessor.processEmail(email)
if (response) {
log.info(`Generated response for ${email.from}`)
this.emit('response', {
to: email.from,
from: this.config.botEmailAddress,
subject: `Re: ${email.subject}`,
body: response,
inReplyTo: email.messageId,
})
}
} catch (error) {
log.error('Failed to process email:', error)
}
}
private listeners: Map<string, Function[]> = new Map()
public on(event: string, callback: Function): void {
if (!this.listeners.has(event)) {
this.listeners.set(event, [])
}
this.listeners.get(event)!.push(callback)
}
public emit(event: string, data: any): void {
const callbacks = this.listeners.get(event) || []
callbacks.forEach(cb => cb(data))
}
public getConfig(): DovecotConfig {
return { ...this.config }
}
public updateConfig(config: Partial<DovecotConfig>): void {
this.config = { ...this.config, ...config }
log.info('Configuration updated. Restart required for changes to take effect.')
}
}
export { EmailMessage, MilterConfig, LMTPConfig }