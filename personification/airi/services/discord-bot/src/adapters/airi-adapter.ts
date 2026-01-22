import type { Interaction } from 'discord.js'
import { env } from 'node:process'
import { useLogg } from '@guiiai/logg'
import { Client as AiriClient } from '@proj-airi/server-sdk'
import { Client, Events, GatewayIntentBits } from 'discord.js'
import { handlePing, registerCommands, VoiceManager } from '../bots/discord/commands'
const log = useLogg('DiscordAdapter')
export interface DiscordAdapterConfig {
  discordToken?: string
  airiToken?: string
  airiUrl?: string
}
interface DiscordConfig {
  token?: string
  enabled?: boolean
}
function isDiscordConfig(config: unknown): config is DiscordConfig {
  if (typeof config !== 'object' || config === null)
    return false
  const c = config as Record<string, unknown>
  return (typeof c.token === 'string' || typeof c.token === 'undefined')
    && (typeof c.enabled === 'boolean' || typeof c.enabled === 'undefined')
}
export class DiscordAdapter {
  private airiClient: AiriClient
  private discordClient: Client
  private discordToken: string
  private voiceManager: VoiceManager
  private isReconnecting = false
  constructor(config: DiscordAdapterConfig) {
    this.discordToken = config.discordToken || env.DISCORD_TOKEN || ''
    this.discordClient = new Client({
      intents: [GatewayIntentBits.Guilds, GatewayIntentBits.GuildVoiceStates],
    })
    this.airiClient = new AiriClient({
      name: 'discord-bot',
      possibleEvents: [
        'input:text',
        'input:text:voice',
        'input:voice',
        'ui:configure',
      ],
      token: config.airiToken,
      url: config.airiUrl,
    })
    this.voiceManager = new VoiceManager(this.discordClient, this.airiClient)
    this.setupEventHandlers()
  }
  private setupEventHandlers(): void {
    this.airiClient.onEvent('ui:configure', async (event) => {
      if (event.data.moduleName === 'discord') {
        if (this.isReconnecting) {
          log.warn('A reconnect is already in progress, skipping this configuration event.')
          return
        }
        this.isReconnecting = true
        try {
          log.log('Received Discord configuration:', event.data.config)
          if (isDiscordConfig(event.data.config)) {
            const config = event.data.config as DiscordConfig
            const { token, enabled } = config
            if (enabled === false) {
              if (this.discordClient.isReady) {
                log.log('Disabling Discord bot as per configuration...')
                await this.discordClient.destroy()
              }
              return
            }
            if (!token) {
              log.warn('Discord bot enabled, but no token provided. Stopping bot.')
              if (this.discordClient.isReady) {
                await this.discordClient.destroy()
              }
              return
            }
            if (this.discordToken !== token || !this.discordClient.isReady) {
              this.discordToken = token
              if (this.discordClient.isReady) {
                log.log('Reconnecting Discord client with new token...')
                await this.discordClient.destroy()
              }
              log.log('Connecting Discord client...')
              await this.discordClient.login(this.discordToken)
              log.log('Discord client connected.')
            }
          }
          else {
            log.warn('Invalid Discord configuration received, skipping...')
          }
        }
        catch (error) {
          log.withError(error as Error).error('Failed to apply Discord configuration.')
        }
        finally {
          this.isReconnecting = false
        }
      }
    })
    this.airiClient.onEvent('input:text', async (event) => {
      log.log('Received input from AIRI system:', event.data.text)
    })
    this.discordClient.once(Events.ClientReady, (readyClient) => {
      log.log(`Discord bot ready! User: ${readyClient.user.tag}`)
    })
    this.discordClient.on(Events.InteractionCreate, async (interaction: Interaction) => {
      if (!interaction.isChatInputCommand())
        return
      log.log('Interaction received:', interaction)
      switch (interaction.commandName) {
        case 'ping':
          await handlePing(interaction)
          break
        case 'summon':
          await this.voiceManager.handleJoinChannelCommand(interaction)
          break
      }
    })
  }
  async start(): Promise<void> {
    log.log('Starting Discord adapter...')
    try {
      await registerCommands()
      if (this.discordToken) {
        await this.discordClient.login(this.discordToken)
        log.log('Discord adapter started successfully')
      }
      else {
        log.warn('Discord token not provided. Waiting for configuration from UI.')
      }
    }
    catch (error) {
      log.withError(error).error('Failed to start Discord adapter')
      throw error
    }
  }
  async stop(): Promise<void> {
    log.log('Stopping Discord adapter...')
    try {
      await this.discordClient.destroy()
      this.airiClient.close()
      log.log('Discord adapter stopped')
    }
    catch (error) {
      log.withError(error).error('Error stopping Discord adapter')
      throw error
    }
  }
}