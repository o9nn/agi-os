import langJsx from '@shikijs/langs/jsx'
import langTsx from '@shikijs/langs/tsx'
import langVue from '@shikijs/langs/vue'
import themeLight from '@shikijs/themes/catppuccin-latte'
import themeDark from '@shikijs/themes/catppuccin-mocha'
import { createHighlighterCoreSync } from '@shikijs/core'
import { createJavaScriptRegexEngine } from '@shikijs/engine-javascript'
import { shikiToMonaco } from '@shikijs/monaco'
import * as monaco from 'monaco-editor-core'
let registered = false
export function registerHighlighter() {
if (!registered) {
const highlighter = createHighlighterCoreSync({
themes: [themeDark, themeLight],
langs: [langVue, langTsx, langJsx],
engine: createJavaScriptRegexEngine(),
})
monaco.languages.register({ id: 'vue' })
shikiToMonaco(highlighter, monaco)
registered = true
}
return {
light: themeLight.name!,
dark: themeDark.name!,
}
}