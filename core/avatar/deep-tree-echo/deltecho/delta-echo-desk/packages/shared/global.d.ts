import { getMessageFunction, LocaleData } from './localize.ts'
declare global {
  interface Window {
    localeData: LocaleData
    static_translate: getMessageFunction
  }
}