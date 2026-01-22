export type PromiseType<T> = T extends Promise<infer U> ? U : any
type Bounds = {
  height: number
  width: number
  x: number
  y: number
}
export interface DesktopSettingsType {
  bounds: Bounds | {}
  HTMLEmailWindowBounds: Bounds | undefined
  chatViewBgImg?: string
  credentials?: never
  lastAccount?: number
  enableAVCalls: boolean
  enableBroadcastLists: boolean
  enableChatAuditLog: boolean
  enableOnDemandLocationStreaming: boolean
  enterKeySends: boolean
  locale: string | null
  notifications: boolean
  showNotificationContent: boolean
  isMentionsEnabled: boolean
  lastChats: { [accountId: number]: number }
  zoomFactor: number
  activeTheme: string
  minimizeToTray: boolean
  syncAllAccounts: boolean
  lastSaveDialogLocation: string | undefined
  experimentalEnableMarkdownInMessages: boolean
  enableWebxdcDevTools: boolean
  HTMLEmailAskForRemoteLoadingConfirmation: boolean
  HTMLEmailAlwaysLoadRemoteContent: boolean
  enableRelatedChats: boolean
  galleryImageKeepAspectRatio: boolean
  useSystemUIFont: boolean
  contentProtectionEnabled: boolean
  autostart: boolean
  deepTreeEchoBotEnabled: boolean
  deepTreeEchoBotApiKey?: string
  deepTreeEchoBotApiEndpoint?: string
  deepTreeEchoBotMemoryEnabled: boolean
  deepTreeEchoBotPersonality?: string
  deepTreeEchoBotVisionEnabled: boolean
  deepTreeEchoBotWebAutomationEnabled: boolean
  deepTreeEchoBotEmbodimentEnabled: boolean
  deepTreeEchoBotPersonaState?: string
  deepTreeEchoBotMemories?: string
  deepTreeEchoBotReflections?: string
  deepTreeEchoBotCognitiveKeys?: string
}
export interface RC_Config {
  'log-debug': boolean
  'log-to-console': boolean
  'machine-readable-stacktrace': boolean
  theme: string | undefined
  'theme-watch': boolean
  devmode: boolean
  'translation-watch': boolean
  minimized: boolean
  version: boolean
  v: boolean
  help: boolean
  h: boolean
  'allow-unsafe-core-replacement': boolean
}
import type { T } from '@deltachat/jsonrpc-client'
import { NOTIFICATION_TYPE } from './constants.ts'
export type Theme = {
  name: string
  description: string
  address: string
  is_prototype: boolean
}
export type RuntimeInfo = {
  isMac: boolean
  isAppx: boolean
  target: 'electron' | 'browser' | 'tauri'
  versions: { label: string; value: string }[]
  runningUnderARM64Translation?: boolean
  rpcServerPath?: string
  buildInfo: BuildInfo
  isContentProtectionSupported: boolean
  hideEmojiAndStickerPicker?: boolean
  tauriSpecific?: {
    scheme: {
      blobs: string
      chatBackgroundImage: string
      webxdcIcon: string
      stickers: string
    }
  }
}
export interface BuildInfo {
  VERSION: string
  GIT_REF: string
  BUILD_TIMESTAMP: number
}
export interface DcNotification {
  title: string
  body: string
  icon: string | null
  iconIsAvatar?: boolean 
  chatId: number
  messageId: number
  accountId: number
  notificationType: NOTIFICATION_TYPE
}
export interface DcOpenWebxdcParameters {
  accountId: number
  displayname: string | null
  webxdcInfo: T.WebxdcMessageInfo
  chatName: string
  href: string
}
export interface RuntimeOpenDialogOptions {
  title?: string
  filters?: {
    name: string
    extensions: string[]
  }[]
  properties: (
    | 'openFile'
    | 'openDirectory'
    | 'createDirectory'
    | 'multiSelections'
  )[]
  defaultPath?: string
  buttonLabel?: string
}
export interface AutostartState {
  isSupported: boolean
  isRegistered: boolean
}