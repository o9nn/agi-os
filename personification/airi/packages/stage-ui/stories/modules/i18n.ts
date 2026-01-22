import messages from '@proj-airi/i18n/locales'
import { createI18n } from 'vue-i18n'
const languageRemap: Record<string, string> = {
'zh-CN': 'zh-Hans',
'zh-TW': 'zh-Hans',
'zh-HK': 'zh-Hans',
'zh-Hant': 'zh-Hans',
'en-US': 'en',
'en-GB': 'en',
'en-AU': 'en',
'en': 'en',
'es-ES': 'es',
'es-MX': 'es',
'es-AR': 'es',
'es': 'es',
'ru': 'ru',
'ru-RU': 'ru',
'fr': 'fr',
'fr-FR': 'fr',
}
function getLocale() {
let language = localStorage.getItem('settings/language')
const languages = Object.keys(messages!)
if (languageRemap[language || 'en'] != null) {
language = languageRemap[language || 'en']
}
if (language && languages.includes(language))
return language
return 'en'
}
export const i18n = createI18n({
legacy: false,
locale: getLocale(),
fallbackLocale: 'en',
messages,
})