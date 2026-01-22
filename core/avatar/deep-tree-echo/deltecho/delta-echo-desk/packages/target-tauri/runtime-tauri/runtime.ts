import { Channel, invoke } from '@tauri-apps/api/core'
import { getCurrentWindow } from '@tauri-apps/api/window'
import type { attachLogger } from '@tauri-apps/plugin-log'
import { getStore } from '@tauri-apps/plugin-store'
import type { Store } from '@tauri-apps/plugin-store'
import { openPath, openUrl } from '@tauri-apps/plugin-opener'
import { writeText, readText } from '@tauri-apps/plugin-clipboard-manager'
import type {
  AutostartState,
  DcNotification,
  DcOpenWebxdcParameters,
  DesktopSettingsType,
  RC_Config,
  RuntimeInfo,
  RuntimeOpenDialogOptions,
  Theme,
} from '@deltachat-desktop/shared/shared-types.js'
import '@deltachat-desktop/shared/global.d.ts'
import type {
  MediaAccessStatus,
  MediaType,
  Runtime,
  RuntimeAppPath,
} from '@deltachat-desktop/runtime-interface'
import { BaseDeltaChat, yerpc } from '@deltachat/jsonrpc-client'
import type { LocaleData } from '@deltachat-desktop/shared/localize.js'
import type {
  getLogger as getLoggerFunction,
  LogLevelString,
} from '@deltachat-desktop/shared/logger.js'
import type { setLogHandler as setLogHandlerFunction } from '@deltachat-desktop/shared/logger.js'
let logJsonrpcConnection = false
type MainWindowEvents =
  | {
      event: 'sendToChat'
      data: {
        options: {
          text: string | null | undefined
          file: { fileName: string; fileContent: string } | null
        }
        account: number | null
      }
    }
  | {
      event: 'localeReloaded'
      data: string | null
    }
  | {
      event: 'showAboutDialog'
    }
  | {
      event: 'showSettingsDialog'
    }
  | {
      event: 'showKeybindingsDialog'
    }
  | {
      event: 'resumeFromSleep'
    }
  | {
      event: 'toggleNotifications'
    }
  | {
      event: 'onThemeUpdate'
    }
  | {
      event: 'notificationClick'
      data: { accountId: number; chatId: number; msgId: number }
    }
  | {
      event: 'deepLinkOpened'
      data: string
    }
const events = new Channel<MainWindowEvents>()
const jsonrpc = new Channel<yerpc.Message>()
invoke('set_main_window_channels', { jsonrpc, events })
class TauriTransport extends yerpc.BaseTransport {
  constructor(private callCounterFunction: (label: string) => void) {
    super()
    jsonrpc.onmessage = (message: yerpc.Message) => {
      if (logJsonrpcConnection) {
        console.debug('%c▼ %c[JSONRPC]', 'color: red', 'color:grey', message)
      }
      this._onmessage(message)
    }
  }
  _send(message: yerpc.Message): void {
    invoke('deltachat_jsonrpc_request', { message })
    if (logJsonrpcConnection) {
      console.debug('%c▲ %c[JSONRPC]', 'color: green', 'color:grey', message)
      if ((message as any)['method']) {
        this.callCounterFunction((message as any).method)
        this.callCounterFunction('total')
      }
    }
  }
}
export class TauriDeltaChat extends BaseDeltaChat<TauriTransport> {
  constructor(callCounterFunction: (label: string) => void) {
    super(new TauriTransport(callCounterFunction), true)
  }
}
const isWindowsOS = navigator.userAgent.includes('Win')
class TauriRuntime implements Runtime {
  constructor() {
    this.getActiveTheme = this.getActiveTheme.bind(this)
  }
  emitUIFullyReady(): void {
    invoke('ui_frontend_ready')
  }
  emitUIReady(): void {
    invoke('ui_ready')
  }
  createDeltaChatConnection(
    callCounterFunction: (label: string) => void
  ): BaseDeltaChat<any> {
    return new TauriDeltaChat(callCounterFunction)
  }
  openMessageHTML(
    accountId: number,
    messageId: number,
    isContactRequest: boolean,
    subject: string,
    sender: string,
    receiveTime: string,
    content: string
  ): void {
    invoke('open_html_window', {
      accountId,
      messageId,
      isContactRequest,
      subject,
      sender,
      receiveTime,
      content,
    })
  }
  async getDesktopSettings(): Promise<DesktopSettingsType> {
    const deprecated = {
      credentials: undefined,
      lastAccount: undefined,
      lastChats: {},
    } satisfies Partial<DesktopSettingsType>
    const static_backend = {
      ...deprecated,
      bounds: {}, 
      HTMLEmailWindowBounds: undefined, 
    } satisfies Partial<DesktopSettingsType>
    const frontendAndTauri = {
      zoomFactor: 1, 
      minimizeToTray: true,
      lastSaveDialogLocation: undefined,
      enableWebxdcDevTools: false, 
      HTMLEmailAskForRemoteLoadingConfirmation: true,
      HTMLEmailAlwaysLoadRemoteContent: false,
      contentProtectionEnabled: false,
      activeTheme: 'system',
      locale: null, 
      notifications: true,
      syncAllAccounts: true,
      autostart: true,
      deepTreeEchoBotEnabled: false,
      deepTreeEchoBotMemoryEnabled: false,
      deepTreeEchoBotPersonality: '',
      deepTreeEchoBotApiKey: '',
      deepTreeEchoBotApiEndpoint: '',
      deepTreeEchoBotVisionEnabled: false,
      deepTreeEchoBotWebAutomationEnabled: false,
      deepTreeEchoBotEmbodimentEnabled: false,
      deepTreeEchoBotPersonaState: '',
      deepTreeEchoBotMemories: '',
      deepTreeEchoBotReflections: '',
      deepTreeEchoBotCognitiveKeys: '',
    } satisfies Partial<DesktopSettingsType>
    const frontendOnly = {
      showNotificationContent: true,
      enterKeySends: false,
      enableAVCalls: false,
      enableBroadcastLists: false,
      enableChatAuditLog: false,
      enableOnDemandLocationStreaming: false,
      chatViewBgImg: undefined,
      experimentalEnableMarkdownInMessages: false,
      enableRelatedChats: false,
      galleryImageKeepAspectRatio: false,
      isMentionsEnabled: false,
      useSystemUIFont: false,
    } satisfies Partial<DesktopSettingsType>
    const savedEntries = (await this.store.entries()).reduce(
      (acc, [key, value]) => {
        ;(acc as any)[key] = value
        return acc
      },
      {} as Partial<DesktopSettingsType>
    )
    return {
      ...static_backend,
      ...frontendAndTauri,
      ...frontendOnly,
      ...savedEntries,
    } satisfies DesktopSettingsType
  }
  async setDesktopSetting(
    key: keyof DesktopSettingsType,
    value: string | number | boolean | undefined
  ): Promise<void> {
    if (typeof value === 'undefined') {
      await this.store.delete(key)
    } else {
      await this.store.set(key, value)
    }
    await invoke('change_desktop_settings_apply_side_effects', { key })
  }
  private log!: ReturnType<typeof getLoggerFunction>
  private store!: Store
  async initialize(
    setLogHandler: typeof setLogHandlerFunction,
    getLogger: typeof getLoggerFunction
  ): Promise<void> {
    const config = await invoke<{
      log_debug: boolean
      log_to_console: boolean
      devtools: boolean
      dev_mode: boolean
      forced_tray_icon: boolean
      theme: string | null
      theme_watch: boolean
    }>('get_frontend_run_config')
    const rc_config: RC_Config = {
      'log-debug': config.log_debug,
      'log-to-console': config.log_to_console,
      devmode: config.dev_mode,
      minimized: config.forced_tray_icon,
      theme: config.theme || undefined,
      'theme-watch': config.theme_watch,
      'translation-watch': false,
      'allow-unsafe-core-replacement': false,
      'machine-readable-stacktrace': true,
      version: false,
      v: false,
      help: false,
      h: false,
    }
    this.rc_config = rc_config
    if (rc_config['log-debug']) {
      logJsonrpcConnection = true
    }
    const runtime_info: RuntimeInfo = await invoke('get_runtime_info')
    this.runtime_info = runtime_info
    type TauriLogVariants = Parameters<
      Parameters<typeof attachLogger>[0]
    >[0]['level']
    const variants: Record<LogLevelString, TauriLogVariants> = {
      DEBUG: 2,
      INFO: 3,
      WARNING: 4,
      ERROR: 5,
      CRITICAL: 5,
    }
    setLogHandler((channel, level, _stack_trace, ...args) => {
      const message = args
        .map(arg => (typeof arg === 'object' ? JSON.stringify(arg) : arg))
        .join(', ')
      const traces = new Error().stack
        ?.split('\n')
        .map(line => line.split('@'))
        .slice(3) 
      const filtered = traces?.filter(([name, location]) => {
        return name.length > 0 && location !== '[native code]'
      })
      let location = filtered?.[0]?.filter(v => v.length > 0).join('@')
      if (location === 'Error') {
        location = 'webview::unknown'
      }
      const onlyFnName = location?.split('@')[0]
      location = `:JS::${channel.replace(/\
        onlyFnName ? `::${onlyFnName}` : ''
      }`
      const tauriLogLevel = variants[level]
      invoke('plugin:log|log', {
        level: tauriLogLevel,
        message,
        location,
        file: undefined,
        line: undefined,
        keyValues:
          tauriLogLevel <= variants.ERROR
            ? { stack_trace: JSON.stringify(traces) }
            : undefined,
      })
    }, rc_config)
    this.log = getLogger('runtime/tauri')
    const store = await getStore('config.json')
    if (!store) {
      throw new Error('Configuration Store was not loaded')
    }
    this.store = store
    this.currentLogFileLocation = await invoke('get_current_logfile')
    events.onmessage = event => {
      if (event.event === 'sendToChat') {
        const { options, account } = event.data
        this.onWebxdcSendToChat?.(
          options.file
            ? {
                file_name: options.file.fileName,
                file_content: options.file.fileContent,
              }
            : null,
          options.text || null,
          account || undefined
        )
      } else if (event.event === 'localeReloaded') {
        this.onChooseLanguage?.(event.data || window.localeData.locale)
      } else if (event.event === 'showAboutDialog') {
        this.onShowDialog?.('about')
      } else if (event.event === 'showSettingsDialog') {
        this.onShowDialog?.('settings')
      } else if (event.event === 'showKeybindingsDialog') {
        this.onShowDialog?.('keybindings')
      } else if (event.event === 'resumeFromSleep') {
        this.onResumeFromSleep?.()
      } else if (event.event === 'toggleNotifications') {
        this.onToggleNotifications?.()
      } else if (event.event === 'onThemeUpdate') {
        this.log.debug('on theme update')
        this.onThemeUpdate?.()
      } else if (event.event === 'deepLinkOpened') {
        this.onOpenQrUrl?.(event.data)
      } else if (event.event === 'notificationClick') {
        this.notificationCallback?.(event.data)
      }
    }
    window
      .matchMedia('(prefers-color-scheme: dark)')
      .addEventListener('change', event => {
        this.log.debug('system theme changed:', { dark_theme: event.matches })
        this.onThemeUpdate?.()
      })
  }
  reloadWebContent(): void {
    location.reload()
  }
  openLogFile(): void {
    openPath(this.getCurrentLogLocation())
  }
  currentLogFileLocation: string | null = null
  getCurrentLogLocation(): string {
    if (this.currentLogFileLocation === null) {
      throw new Error('this.currentLogFileLocation is not set')
    }
    return this.currentLogFileLocation
  }
  openHelpWindow(anchor?: string): void {
    invoke('open_help_window', { locale: window.localeData.locale, anchor })
  }
  private rc_config: RC_Config | null = null
  getRC_Config(): RC_Config {
    if (this.rc_config === null) {
      throw new Error('this.rc_config is not set')
    }
    return this.rc_config
  }
  private runtime_info: RuntimeInfo | null = null
  getRuntimeInfo(): RuntimeInfo {
    if (this.runtime_info === null) {
      throw new Error('this.runtime_info is not set')
    }
    return this.runtime_info
  }
  openLink(link: string): void {
    if (link.startsWith('http:') || link.startsWith('https:')) {
      openUrl(link)
    } else {
      this.log.error('tried to open a non http/https external link', {
        link,
      })
    }
  }
  async showOpenFileDialog(
    options: RuntimeOpenDialogOptions
  ): Promise<string[]> {
    return await invoke('show_open_file_dialog', {
      title: options.title,
      filters: options.filters,
      properties: options.properties,
      defaultPath: options.defaultPath,
    })
  }
  async downloadFile(pathToSource: string, filename: string): Promise<void> {
    await invoke('download_file', { pathToSource, filename })
  }
  transformBlobURL(blob_path: string): string {
    const matches = blob_path.match(/.*(:?\\|\/)(.+?)\1dc.db-blobs\1(.*)/)
    if (matches) {
      const filename = encodeURIComponent(matches[3])
      return `${this.runtime_info?.tauriSpecific?.scheme.blobs}${matches[2]}/${filename}`
    }
    if (blob_path !== '') {
      this.log.error('transformBlobURL wrong url format', blob_path)
    } else {
      this.log.debug('transformBlobURL called with empty string for blob_path')
    }
    return ''
  }
  transformStickerURL(sticker_path: string): string {
    const matches = sticker_path.match(
      /.*(:?\\|\/)(.+?)\1stickers\1(.+?)\1(.+)/
    )
    if (matches) {
      const packName = encodeURIComponent(matches[3])
      const filename = encodeURIComponent(matches[4])
      return `${this.runtime_info?.tauriSpecific?.scheme.stickers}${matches[2]}/${packName}/${filename}`
    }
    if (sticker_path !== '') {
      this.log.error('transformStickerURL wrong url format', sticker_path)
    } else {
      this.log.debug(
        'transformStickerURL called with empty string for sticker_path'
      )
    }
    return ''
  }
  readClipboardText(): Promise<string> {
    return readText()
  }
  readClipboardImage(): Promise<string | null> {
    return invoke('get_clipboard_image_as_data_uri')
  }
  writeClipboardText(text: string): Promise<void> {
    return writeText(text)
  }
  writeClipboardImage(path: string): Promise<void> {
    return invoke('copy_image_to_clipboard', { path })
  }
  getAppPath(name: RuntimeAppPath): Promise<string> {
    return invoke('get_app_path', { name })
  }
  openMapsWebxdc(_accountId: number, _chatId?: number): void {
    throw new Error('Method not implemented.22')
  }
  async openPath(path: string): Promise<string> {
    try {
      await openPath(path)
      return ''
    } catch (error: any) {
      this.log.error('openPath', path, error)
      return error?.message || error.toString()
    }
  }
  getConfigPath(): string {
    throw new Error('Method not implemented.24')
  }
  openWebxdc(messageId: number, params: DcOpenWebxdcParameters): void {
    invoke('open_webxdc', {
      messageId,
      accountId: params.accountId,
      href: params.href,
    })
  }
  getWebxdcIconURL(accountId: number, msgId: number): string {
    return `${this.runtime_info?.tauriSpecific?.scheme.webxdcIcon}${accountId}/${msgId}`
  }
  deleteWebxdcAccountData(accountId: number): Promise<void> {
    return invoke('delete_webxdc_account_data', { accountId })
  }
  closeAllWebxdcInstances(): void {
    invoke('close_all_webxdc_instances')
  }
  notifyWebxdcStatusUpdate(accountId: number, instanceId: number): void {
    invoke('on_webxdc_status_update', { accountId, instanceId })
  }
  notifyWebxdcRealtimeData(
    accountId: number,
    instanceId: number,
    payload: number[]
  ): void {
    invoke('on_webxdc_realtime_data', { accountId, instanceId, payload })
  }
  notifyWebxdcMessageChanged(accountId: number, instanceId: number): void {
    invoke('on_webxdc_message_changed', { accountId, instanceId })
  }
  notifyWebxdcInstanceDeleted(accountId: number, instanceId: number): void {
    invoke('on_webxdc_message_deleted', { accountId, instanceId })
  }
  restartApp(): void {
    this.log.error('Method not implemented: restartApp')
  }
  async getLocaleData(locale?: string): Promise<LocaleData> {
    return await invoke('get_locale_data', {
      locale: locale || (await this.getDesktopSettings()).locale || 'en',
    })
  }
  setLocale(locale: string): Promise<void> {
    return invoke('change_lang', { locale })
  }
  setBadgeCounter(value: number): void {
    const window = getCurrentWindow()
    window.setBadgeCount(value === 0 ? undefined : value)
    if (isWindowsOS) {
      window.setOverlayIcon?.(
        value === 0 ? undefined : 'images/tray/unread-badge.png'
      )
    }
    invoke('update_tray_icon_badge', { counter: value })
  }
  showNotification({
    title,
    body,
    icon,
    iconIsAvatar,
    chatId,
    messageId,
    accountId,
  }: DcNotification): void {
    invoke('show_notification', {
      title,
      body,
      icon,
      iconIsAvatar: iconIsAvatar || false,
      chatId,
      messageId,
      accountId,
    })
  }
  clearAllNotifications(): void {
    invoke('clear_all_notifications')
  }
  clearNotifications(accountId: number, chatId: number): void {
    invoke('clear_notifications', { accountId, chatId })
  }
  notificationCallback?: (data: {
    accountId: number
    chatId: number
    msgId: number
  }) => void
  setNotificationCallback(
    cb: (data: { accountId: number; chatId: number; msgId: number }) => void
  ): void {
    this.notificationCallback = cb
  }
  writeTempFileFromBase64(name: string, content: string): Promise<string> {
    return invoke('write_temp_file_from_base64', { name, content })
  }
  writeTempFile(name: string, content: string): Promise<string> {
    return invoke('write_temp_file', { name, content })
  }
  copyFileToInternalTmpDir(
    fileName: string,
    sourcePath: string
  ): Promise<string> {
    return invoke('copy_blob_file_to_internal_tmp_dir', {
      fileName,
      sourcePath,
    })
  }
  removeTempFile(path: string): Promise<void> {
    return invoke('remove_temp_file', { path })
  }
  getWebxdcDiskUsage(
    _accountId: number
  ): Promise<{ total_size: number; data_size: number }> {
    throw new Error('Method not implemented: runtime.getWebxdcDiskUsage')
  }
  clearWebxdcDOMStorage(_accountId: number): Promise<void> {
    throw new Error('Method not implemented.46')
  }
  getAvailableThemes(): Promise<Theme[]> {
    return invoke<Theme[]>('get_available_themes')
  }
  async getActiveTheme(): Promise<{ theme: Theme; data: string } | null> {
    let themeAddress = await invoke<string>('get_current_active_theme_address')
    if (themeAddress === 'system') {
      if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
        themeAddress = 'dc:dark'
      } else {
        themeAddress = 'dc:light'
      }
    }
    try {
      const [theme, theme_content] = await invoke<
        [theme: Theme, theme_content: string]
      >('get_theme', { themeAddress })
      return { theme, data: theme_content }
    } catch (err) {
      this.log.error('failed to getActiveTheme:', err)
      return null
    }
  }
  saveBackgroundImage(
    srcPath: string,
    isDefaultPicture: boolean
  ): Promise<string> {
    return invoke('copy_background_image_file', { srcPath, isDefaultPicture })
  }
  onDragFileOut(_file: string): void {
    throw new Error('Method not implemented.50')
  }
  isDroppedFileFromOutside(_file: File): boolean {
    throw new Error('Method not implemented.51')
  }
  async debug_get_datastore_ids() {
    return await invoke('debug_get_datastore_ids')
  }
  getAutostartState(): Promise<AutostartState> {
    return invoke('get_autostart_state')
  }
  onChooseLanguage: ((locale: string) => Promise<void>) | undefined
  onThemeUpdate: (() => void) | undefined
  onShowDialog:
    | ((kind: 'about' | 'keybindings' | 'settings') => void)
    | undefined
  onOpenQrUrl: ((url: string) => void) | undefined
  onWebxdcSendToChat:
    | ((
        file: { file_name: string; file_content: string } | null,
        text: string | null,
        account_id?: number
      ) => void)
    | undefined
  onResumeFromSleep: (() => void) | undefined
  onToggleNotifications: (() => void) | undefined
  checkMediaAccess(_mediaType: MediaType): Promise<MediaAccessStatus> {
    throw new Error('Method not implemented.')
  }
  askForMediaAccess(_mediaType: MediaType): Promise<boolean> {
    throw new Error('Method not implemented.')
  }
}
;(window as any).r = new TauriRuntime()