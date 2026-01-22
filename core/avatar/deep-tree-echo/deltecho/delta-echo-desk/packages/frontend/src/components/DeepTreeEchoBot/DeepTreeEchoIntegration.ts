import { getLogger } from '@deltachat-desktop/shared/logger'
import { BackendRemote, onDCEvent, Type as T } from '../../backend-com'
import { runtime } from '@deltachat-desktop/runtime-interface'
import { DeepTreeEchoBot, DeepTreeEchoBotOptions } from './DeepTreeEchoBot'
const log = getLogger(
  'render/components/DeepTreeEchoBot/DeepTreeEchoIntegration'
)
let botInstance: DeepTreeEchoBot | null = null
export async function initDeepTreeEchoBot(): Promise<void> {
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
    registerMessageHandlers()
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
function registerMessageHandlers(): void {
  if (!botInstance) return
  onDCEvent('DcEventNewMsg', (accountId, chatId, msgId) => {
    handleNewMessage(accountId, chatId, msgId)
  })
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
export async function saveBotSettings(settings: any): Promise<void> {
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
      await runtime.setDesktopSetting(settingKey, value)
    }
    if (botInstance) {
      botInstance.updateOptions(settings)
    }
    else if (settings.enabled) {
      await initDeepTreeEchoBot()
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
initDeepTreeEchoBot()