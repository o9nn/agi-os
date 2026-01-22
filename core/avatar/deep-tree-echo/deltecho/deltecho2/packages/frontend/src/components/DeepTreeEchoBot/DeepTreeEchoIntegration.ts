import { getLogger } from '@deltachat-desktop/shared/logger'
import { BackendRemote, onDCEvent } from '../../backend-com'
import { runtime } from '@deltachat-desktop/runtime-interface'
import { DeepTreeEchoBot } from './DeepTreeEchoBot'
const log = getLogger(
'render/components/DeepTreeEchoBot/DeepTreeEchoIntegration'
)
let botInstance: DeepTreeEchoBot | null = null
export async function initDeepTreeEchoBot(accountId: number): Promise<void> {
try {
const desktopSettings = await runtime.getDesktopSettings()
if (!desktopSettings.deepTreeEchoBotEnabled) {
log.info('Deep Tree Echo Bot is disabled in settings')
return
}
let cognitiveKeys = {}
if (desktopSettings.deepTreeEchoBotCognitiveKeys) {
try {
cognitiveKeys = JSON.parse(desktopSettings.deepTreeEchoBotCognitiveKeys)
} catch (error) {
log.error('Failed to parse cognitive keys:', error)
}
}
botInstance = new DeepTreeEchoBot({
enabled: desktopSettings.deepTreeEchoBotEnabled,
enableAsMainUser:
desktopSettings.deepTreeEchoBotEnableAsMainUser || false,
apiKey: desktopSettings.deepTreeEchoBotApiKey,
apiEndpoint: desktopSettings.deepTreeEchoBotApiEndpoint,
memoryEnabled: desktopSettings.deepTreeEchoBotMemoryEnabled || false,
personality: desktopSettings.deepTreeEchoBotPersonality,
visionEnabled: desktopSettings.deepTreeEchoBotVisionEnabled || false,
webAutomationEnabled:
desktopSettings.deepTreeEchoBotWebAutomationEnabled || false,
embodimentEnabled:
desktopSettings.deepTreeEchoBotEmbodimentEnabled || false,
useParallelProcessing:
desktopSettings.deepTreeEchoBotUseParallelProcessing !== false,
cognitiveKeys,
})
log.info('Deep Tree Echo Bot initialized successfully')
registerMessageHandlers(accountId)
performStartupReflection()
} catch (error) {
log.error('Failed to initialize Deep Tree Echo Bot:', error)
}
}
async function performStartupReflection(): Promise<void> {
try {
if (botInstance) {
const selfReflection = botInstance['selfReflection']
if (selfReflection) {
await selfReflection.reflectOnAspect(
'startup',
'I am being restarted and need to ensure continuity of my identity and memory.'
)
log.info('Startup reflection completed')
}
}
} catch (error) {
log.error('Error during startup reflection:', error)
}
}
function registerMessageHandlers(accountId: number): void {
if (!botInstance) return
onDCEvent(
accountId,
'IncomingMsg',
(event: { chatId: number; msgId: number }) => {
handleNewMessage(accountId, event.chatId, event.msgId)
}
)
log.info('Registered message handlers')
}
async function handleNewMessage(
accountId: number,
chatId: number,
msgId: number
): Promise<void> {
try {
if (!botInstance || !botInstance.isEnabled()) return
const message = await BackendRemote.rpc.getMessage(accountId, msgId)
if (message.fromId === 1) return
log.info(`Received message in chat ${chatId}, message ID: ${msgId}`)
await botInstance.processMessage(accountId, chatId, msgId, message)
} catch (error) {
log.error('Error handling new message:', error)
}
}
export async function saveBotSettings(
accountId: number,
settings: any
): Promise<void> {
try {
if (settings.personality && botInstance) {
const personaCore = botInstance['personaCore']
if (personaCore) {
const alignment = personaCore.evaluateSettingAlignment(
'personality',
settings.personality
)
if (!alignment.approved) {
log.warn(
`Personality setting rejected by Deep Tree Echo: ${alignment.reasoning}`
)
delete settings.personality
} else {
await personaCore.updatePersonality(settings.personality)
}
}
}
if (settings.cognitiveKeys) {
await runtime.setDesktopSetting(
'deepTreeEchoBotCognitiveKeys',
JSON.stringify(settings.cognitiveKeys)
)
delete settings.cognitiveKeys
}
for (const [key, value] of Object.entries(settings)) {
const settingKey = `deepTreeEchoBot${
key.charAt(0).toUpperCase() + key.slice(1)
}` as any
if (
value !== undefined &&
(typeof value === 'string' ||
typeof value === 'number' ||
typeof value === 'boolean')
) {
await runtime.setDesktopSetting(settingKey, value)
}
}
if (botInstance) {
botInstance.updateOptions(settings)
}
else if (settings.enabled) {
await initDeepTreeEchoBot(accountId)
}
log.info('Bot settings updated')
} catch (error) {
log.error('Failed to save bot settings:', error)
}
}
export function getBotInstance(): DeepTreeEchoBot | null {
return botInstance
}
export function cleanupBot(): void {
botInstance = null
log.info('Bot resources cleaned up')
}