import {
  AutostartState,
  DcNotification,
  DcOpenWebxdcParameters,
  DesktopSettingsType,
  RC_Config,
  RuntimeInfo,
  RuntimeOpenDialogOptions,
  Theme,
} from '@deltachat-desktop/shared/shared-types.js'
import { LocaleData } from '@deltachat-desktop/shared/localize.js'
import { BaseDeltaChat } from '@deltachat/jsonrpc-client'
import type { getLogger as getLoggerFunction } from '@deltachat-desktop/shared/logger.js'
import type { setLogHandler as setLogHandlerFunction } from '@deltachat-desktop/shared/logger.js'
export type MediaType = 'microphone' | 'camera'
export type MediaAccessStatus =
  | 'not-determined'
  | 'granted'
  | 'denied'
  | 'restricted'
  | 'unknown'
export interface Runtime {
  emitUIFullyReady(): void
  emitUIReady(): void
  createDeltaChatConnection(
    callCounterFunction: (label: string) => void
  ): BaseDeltaChat<any>
  openMessageHTML(
    accountId: number,
    messageId: number,
    isContactRequest: boolean,
    subject: string,
    sender: string,
    receiveTime: string,
    content: string
  ): void
  getDesktopSettings(): Promise<DesktopSettingsType>
  setDesktopSetting(
    key: keyof DesktopSettingsType,
    value: string | number | boolean | undefined
  ): Promise<void>
  initialize(
    setLogHandler: typeof setLogHandlerFunction,
    getLogger: typeof getLoggerFunction
  ): Promise<void>
  reloadWebContent(): void
  openLogFile(): void
  getCurrentLogLocation(): string
  openHelpWindow(anchor?: string): void
  getRC_Config(): RC_Config
  getRuntimeInfo(): RuntimeInfo
  openLink(link: string): void
  showOpenFileDialog(options: RuntimeOpenDialogOptions): Promise<string[]>
  downloadFile(pathToSource: string, filename: string): Promise<void>
  transformBlobURL(blob: string): string
  transformStickerURL(sticker_path: string): string
  readClipboardText(): Promise<string>
  readClipboardImage(): Promise<string | null>
  writeClipboardText(text: string): Promise<void>
  writeClipboardImage(path: string): Promise<void>
  getAppPath(name: RuntimeAppPath): Promise<string>
  openMapsWebxdc(accountId: number, chatId?: number): void
  openPath(path: string): Promise<string>
  getConfigPath(): string 
  openWebxdc(msgId: number, params: DcOpenWebxdcParameters): void
  getWebxdcIconURL(accountId: number, msgId: number): string
  deleteWebxdcAccountData(accountId: number): Promise<void>
  closeAllWebxdcInstances(): void
  notifyWebxdcStatusUpdate(accountId: number, instanceId: number): void
  notifyWebxdcRealtimeData(
    accountId: number,
    instanceId: number,
    payload: number[]
  ): void
  notifyWebxdcMessageChanged(accountId: number, instanceId: number): void
  notifyWebxdcInstanceDeleted(accountId: number, instanceId: number): void
  restartApp(): void
  getLocaleData(locale?: string): Promise<LocaleData>
  setLocale(locale: string): Promise<void>
  setBadgeCounter(value: number): void
  showNotification(data: DcNotification): void
  clearAllNotifications(): void
  clearNotifications(accountId: number, chatId: number): void
  setNotificationCallback(
    cb: (data: { accountId: number; chatId: number; msgId: number }) => void
  ): void
  writeTempFileFromBase64(name: string, content: string): Promise<string>
  writeTempFile(name: string, content: string): Promise<string>
  copyFileToInternalTmpDir(
    fileName: string,
    sourcePath: string
  ): Promise<string>
  removeTempFile(path: string): Promise<void>
  getWebxdcDiskUsage(accountId: number): Promise<{
    total_size: number
    data_size: number
  }>
  clearWebxdcDOMStorage(accountId: number): Promise<void>
  getAvailableThemes(): Promise<Theme[]>
  getActiveTheme(): Promise<{
    theme: Theme
    data: string
  } | null>
  saveBackgroundImage(file: string, isDefaultPicture: boolean): Promise<string>
  onDragFileOut(file: string): void
  isDroppedFileFromOutside(file: File): boolean
  getAutostartState(): Promise<AutostartState>
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
        account?: number
      ) => void)
    | undefined
  onResumeFromSleep: (() => void) | undefined
  onToggleNotifications: (() => void) | undefined
  checkMediaAccess: (mediaType: MediaType) => Promise<MediaAccessStatus>
  askForMediaAccess: (mediaType: MediaType) => Promise<boolean | undefined>
}
export const runtime: Runtime = (window as any).r
delete (window as any).r
export type RuntimeAppPath =
  | 'home'
  | 'desktop'
  | 'documents'
  | 'downloads'
  | 'pictures'