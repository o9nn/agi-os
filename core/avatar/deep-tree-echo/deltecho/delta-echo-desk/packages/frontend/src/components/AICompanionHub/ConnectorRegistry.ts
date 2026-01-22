import { EventEmitter } from 'events'
import { runtime } from '@deltachat-desktop/runtime-interface'
import { AIMemory, MemorySystem } from './MemoryPersistenceLayer'
import {
BaseConnector,
AIConnectorConfig,
AICapability,
ConversationContext,
Message,
} from './connectors/BaseConnector'
import { ClaudeConnector, ClaudeConfig } from './connectors/ClaudeConnector'
import { ChatGPTConnector, ChatGPTConfig } from './connectors/ChatGPTConnector'
import {
CharacterAIConnector,
CharacterAIConfig,
} from './connectors/CharacterAIConnector'
import { CopilotConnector, CopilotConfig } from './connectors/CopilotConnector'
import { DeepTreeEchoConnector } from '../AICompanionHub/AIPlatformConnector'
export enum ConnectorRegistryEvent {
CONNECTOR_ADDED = 'connector_added',
CONNECTOR_REMOVED = 'connector_removed',
CONNECTOR_UPDATED = 'connector_updated',
CONNECTOR_AUTHENTICATED = 'connector_authenticated',
CONNECTOR_ERROR = 'connector_error',
MEMORY_ADDED = 'memory_added',
MEMORY_UPDATED = 'memory_updated',
REGISTRY_READY = 'registry_ready',
}
export interface ConnectorInfo {
id: string
name: string
type: string
status: 'online' | 'offline' | 'error' | 'initializing'
capabilities: AICapability[]
personalityTraits: Record<string, number>
conversationCount: number
memoryCount: number
lastActive?: number
error?: string
avatarUrl?: string
}
export class ConnectorRegistry extends EventEmitter {
private static instance: ConnectorRegistry
private connectors: Map<string, BaseConnector> = new Map()
private connectorConfigs: Map<string, AIConnectorConfig> = new Map()
private isInitialized: boolean = false
private activeConnectors: Set<string> = new Set()
private constructor() {
super()
MemorySystem.on('memoryAdded', memory => {
this.emit(ConnectorRegistryEvent.MEMORY_ADDED, memory)
})
MemorySystem.on('memoryUpdated', memory => {
this.emit(ConnectorRegistryEvent.MEMORY_UPDATED, memory)
})
}
public static getInstance(): ConnectorRegistry {
if (!ConnectorRegistry.instance) {
ConnectorRegistry.instance = new ConnectorRegistry()
}
return ConnectorRegistry.instance
}
public async initialize(): Promise<void> {
if (this.isInitialized) return
try {
await MemorySystem.initialize()
const settings = await runtime.getDesktopSettings()
const savedConnectors = settings.aiConnectors || []
for (const config of savedConnectors) {
await this.createConnector(config)
}
this.isInitialized = true
this.emit(ConnectorRegistryEvent.REGISTRY_READY, {
connectorCount: this.connectors.size,
})
console.log(
`AI Connector Registry initialized with ${this.connectors.size} connectors`
)
} catch (error) {
console.error('Failed to initialize AI Connector Registry:', error)
throw error
}
}
public async createConnector(
config: AIConnectorConfig
): Promise<BaseConnector> {
if (!this.isInitialized) {
await this.initialize()
}
if (this.connectors.has(config.id)) {
throw new Error(`Connector with ID ${config.id} already exists`)
}
let connector: BaseConnector
switch (config.type) {
case 'claude':
connector = new ClaudeConnector(config as ClaudeConfig)
break
case 'chatgpt':
connector = new ChatGPTConnector(config as ChatGPTConfig)
break
case 'character-ai':
connector = new CharacterAIConnector(config as CharacterAIConfig)
break
case 'copilot':
connector = new CopilotConnector(config as CopilotConfig)
break
case 'deep-tree-echo':
connector = new DeepTreeEchoConnector(config)
break
default:
throw new Error(`Unknown connector type: ${config.type}`)
}
this.connectors.set(config.id, connector)
this.connectorConfigs.set(config.id, config)
this.setupConnectorEventListeners(connector)
await this.saveConnectorConfigs()
this.emit(ConnectorRegistryEvent.CONNECTOR_ADDED, {
id: config.id,
type: config.type,
name: config.name,
})
return connector
}
public getConnector(id: string): BaseConnector | undefined {
return this.connectors.get(id)
}
public getAllConnectors(): BaseConnector[] {
return Array.from(this.connectors.values())
}
public async getConnectorInfos(): Promise<ConnectorInfo[]> {
const infos: ConnectorInfo[] = []
for (const [id, connector] of this.connectors.entries()) {
const config = this.connectorConfigs.get(id)
if (!config) continue
const conversations = connector.getConversations()
const memories = await MemorySystem.getMemoriesByCompanion(id)
infos.push({
id,
name: config.name,
type: config.type,
status: this.activeConnectors.has(id) ? 'online' : 'offline',
capabilities: config.capabilities,
personalityTraits: config.personalityTraits,
conversationCount: conversations.length,
memoryCount: memories.length,
lastActive:
memories.length > 0
? Math.max(...memories.map(m => m.timestamp))
: undefined,
avatarUrl: config.avatar,
})
}
return infos
}
public async updateConnector(
id: string,
updates: Partial<AIConnectorConfig>
): Promise<void> {
const connector = this.connectors.get(id)
if (!connector) {
throw new Error(`Connector with ID ${id} not found`)
}
const config = this.connectorConfigs.get(id)
if (!config) {
throw new Error(`Configuration for connector ${id} not found`)
}
const updatedConfig = { ...config, ...updates }
this.connectorConfigs.set(id, updatedConfig)
connector.updateConfig(updatedConfig)
await this.saveConnectorConfigs()
this.emit(ConnectorRegistryEvent.CONNECTOR_UPDATED, {
id,
updates,
})
}
public async removeConnector(id: string): Promise<boolean> {
const connector = this.connectors.get(id)
if (!connector) {
return false
}
this.connectors.delete(id)
this.connectorConfigs.delete(id)
this.activeConnectors.delete(id)
await this.saveConnectorConfigs()
this.emit(ConnectorRegistryEvent.CONNECTOR_REMOVED, { id })
return true
}
public async authenticateConnector(id: string): Promise<boolean> {
const connector = this.connectors.get(id)
if (!connector) {
throw new Error(`Connector with ID ${id} not found`)
}
try {
const success = await connector.authenticate()
if (success) {
this.activeConnectors.add(id)
} else {
this.activeConnectors.delete(id)
}
return success
} catch (error) {
console.error(`Error authenticating connector ${id}:`, error)
this.activeConnectors.delete(id)
this.emit(ConnectorRegistryEvent.CONNECTOR_ERROR, {
id,
error: error instanceof Error ? error.message : String(error),
})
return false
}
}
public async sendMessage(
connectorId: string,
conversationId: string,
message: string
): Promise<string> {
const connector = this.connectors.get(connectorId)
if (!connector) {
throw new Error(`Connector with ID ${connectorId} not found`)
}
if (!this.activeConnectors.has(connectorId)) {
const success = await this.authenticateConnector(connectorId)
if (!success) {
throw new Error(`Failed to authenticate connector ${connectorId}`)
}
}
try {
const response = await connector.sendMessage(conversationId, message)
return response.content
} catch (error) {
console.error(`Error sending message to connector ${connectorId}:`, error)
this.emit(ConnectorRegistryEvent.CONNECTOR_ERROR, {
id: connectorId,
error: error instanceof Error ? error.message : String(error),
})
throw error
}
}
public getConversation(
connectorId: string,
conversationId: string
): ConversationContext | undefined {
const connector = this.connectors.get(connectorId)
if (!connector) {
return undefined
}
return connector.getConversation(conversationId)
}
public getAllConversations(connectorId: string): ConversationContext[] {
const connector = this.connectors.get(connectorId)
if (!connector) {
return []
}
return connector.getConversations()
}
public startNewConversation(
connectorId: string,
initialMessage?: string
): string {
const conversationId = `conv_${Date.now()}_${Math.random()
.toString(36)
.substring(2, 7)}`
if (initialMessage) {
this.sendMessage(connectorId, conversationId, initialMessage).catch(
error => {
console.error(
`Error sending initial message to conversation ${conversationId}:`,
error
)
}
)
}
return conversationId
}
public async getMemories(connectorId: string): Promise<AIMemory[]> {
return await MemorySystem.getMemoriesByCompanion(connectorId)
}
public async searchMemories(
query: string,
connectorId?: string
): Promise<AIMemory[]> {
return await MemorySystem.searchMemories(query, connectorId)
}
public async findRelatedMemories(
memoryId: string,
limit?: number
): Promise<AIMemory[]> {
return await MemorySystem.findRelatedMemories(memoryId, limit)
}
private async saveConnectorConfigs(): Promise<void> {
try {
const settings = await runtime.getDesktopSettings()
const connectorConfigs = Array.from(this.connectorConfigs.values())
const updatedSettings = {
...settings,
aiConnectors: connectorConfigs,
}
await runtime.setDesktopSettings(updatedSettings)
} catch (error) {
console.error('Failed to save connector configurations:', error)
throw error
}
}
private setupConnectorEventListeners(connector: BaseConnector): void {
connector.on('authenticated', () => {
const id = this.findConnectorId(connector)
if (id) {
this.activeConnectors.add(id)
this.emit(ConnectorRegistryEvent.CONNECTOR_AUTHENTICATED, { id })
}
})
connector.on('authenticationFailed', error => {
const id = this.findConnectorId(connector)
if (id) {
this.activeConnectors.delete(id)
this.emit(ConnectorRegistryEvent.CONNECTOR_ERROR, {
id,
error: error instanceof Error ? error.message : String(error),
})
}
})
connector.on('error', error => {
const id = this.findConnectorId(connector)
if (id) {
this.emit(ConnectorRegistryEvent.CONNECTOR_ERROR, {
id,
error: error instanceof Error ? error.message : String(error),
})
}
})
connector.on('configUpdated', config => {
const id = this.findConnectorId(connector)
if (id) {
this.connectorConfigs.set(id, config)
this.saveConnectorConfigs().catch(error => {
console.error('Failed to save connector configuration update:', error)
})
this.emit(ConnectorRegistryEvent.CONNECTOR_UPDATED, {
id,
updates: config,
})
}
})
}
private findConnectorId(connector: BaseConnector): string | null {
for (const [id, conn] of this.connectors.entries()) {
if (conn === connector) {
return id
}
}
return null
}
public async shutdown(): Promise<void> {
await MemorySystem.shutdown()
this.isInitialized = false
this.connectors.clear()
this.connectorConfigs.clear()
this.activeConnectors.clear()
console.log('AI Connector Registry shut down')
}
}
export const ConnectorRegistryInstance = ConnectorRegistry.getInstance()