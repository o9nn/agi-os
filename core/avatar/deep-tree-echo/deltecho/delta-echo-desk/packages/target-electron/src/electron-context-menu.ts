'use strict'
import electron, { BrowserWindow, MenuItemConstructorOptions } from 'electron'
import { Event } from 'electron/common'
import { tx } from './load-translations.js'
type todo = any
type MenuItem = electron.MenuItemConstructorOptions | electron.MenuItem
const webContents = (win: BrowserWindow) => win.webContents
const removeUnusedMenuItems = (
  menuTemplate: (MenuItem | false | undefined)[]
): MenuItem[] => {
  let notDeletedPreviousElement: MenuItem
  return menuTemplate
    .filter(menuItem => {
      if (!menuItem) {
        return false
      } else if (typeof menuItem === 'object' && menuItem.visible === false) {
        return false
      }
      return true
    })
    .filter((item, index, array) => {
      const menuItem = item as MenuItem
      const items = array as MenuItem[]
      const toDelete =
        menuItem.type === 'separator' &&
        (!notDeletedPreviousElement ||
          index === array.length - 1 ||
          items[index + 1].type === 'separator')
      notDeletedPreviousElement = toDelete
        ? notDeletedPreviousElement
        : menuItem
      return !toDelete
    }) as MenuItem[]
}
const create = (win: BrowserWindow) => {
  const enableSpellChecking = false
  const handleContextMenu = (_event: Event, props: todo) => {
    const { editFlags } = props
    const hasText = props.selectionText.trim().length > 0
    const can = (type: todo) => editFlags[`can${type}`] && hasText
    const defaultActions: {
      [key: string]: () => MenuItemConstructorOptions
    } = {
      separator: () => ({ type: 'separator' }),
      learnSpelling: () => ({
        id: 'learnSpelling',
        label: tx('menu_learn_spelling'),
        visible: Boolean(props.isEditable && hasText && props.misspelledWord),
        click() {
          const target = webContents(win)
          target.session.addWordToSpellCheckerDictionary(props.misspelledWord)
        },
      }),
      cut: () => ({
        id: 'cut',
        label: tx('global_menu_edit_cut_desktop'),
        enabled: can('Cut'),
        visible: props.isEditable,
        click(_menuItem) {
          const target = webContents(win)
          if (target) {
            target.cut()
          } else {
            electron.clipboard.writeText(props.selectionText)
          }
        },
      }),
      copy: () => ({
        id: 'copy',
        label: tx('global_menu_edit_copy_desktop'),
        enabled: can('Copy'),
        visible: props.isEditable || hasText,
        click() {
          const target = webContents(win)
          if (target) {
            target.copy()
          } else {
            electron.clipboard.writeText(props.selectionText)
          }
        },
      }),
      paste: () => ({
        id: 'paste',
        label: tx('global_menu_edit_paste_desktop'),
        enabled: editFlags.canPaste,
        visible: props.isEditable,
        click() {
          const target = webContents(win)
          target.paste()
        },
      }),
      copyLink: () => ({
        id: 'copyLink',
        label: tx('menu_copy_link_to_clipboard'),
        visible: props.linkURL.length !== 0 && props.mediaType === 'none',
        click() {
          electron.clipboard.write({
            bookmark: props.linkText,
            text: props.linkURL,
          })
        },
      }),
      copyImage: () => ({
        id: 'copyImage',
        label: tx('menu_copy_image_to_clipboard'),
        visible: props.mediaType === 'image',
        click() {
          webContents(win).copyImageAt(props.x, props.y)
        },
      }),
    }
    function word(suggestion: string) {
      return {
        id: 'dictionarySuggestions',
        label: suggestion,
        visible: Boolean(props.isEditable && hasText && props.misspelledWord),
        click(menuItem: MenuItemConstructorOptions) {
          if (menuItem.label) {
            const target = webContents(win)
            target.insertText(menuItem.label)
          }
        },
      }
    }
    let dictionarySuggestions = []
    if (enableSpellChecking) {
      if (
        hasText &&
        props.misspelledWord &&
        props.dictionarySuggestions.length > 0
      ) {
        dictionarySuggestions = props.dictionarySuggestions.map(
          (suggestion: string) => word(suggestion)
        )
      } else {
        dictionarySuggestions.push({
          id: 'dictionarySuggestions',
          label: tx('no_spellcheck_suggestions_found'),
          visible: Boolean(hasText && props.misspelledWord),
          enabled: false,
        })
      }
    }
    let menuTemplate: MenuItem[] = [
      dictionarySuggestions.length > 0 && defaultActions.separator(),
      ...dictionarySuggestions,
      defaultActions.separator(),
      enableSpellChecking && defaultActions.learnSpelling(),
      defaultActions.separator(),
      defaultActions.cut(),
      defaultActions.copy(),
      defaultActions.paste(),
      defaultActions.separator(),
      defaultActions.copyImage(),
      defaultActions.separator(),
      defaultActions.copyLink(),
      defaultActions.separator(),
    ]
    menuTemplate = removeUnusedMenuItems(menuTemplate)
    if (menuTemplate.length > 0) {
      const menu = electron.Menu.buildFromTemplate(menuTemplate as MenuItem[])
      menu.popup({ window: win })
    }
  }
  webContents(win).on('context-menu', handleContextMenu)
  return () => {
    if (win.isDestroyed()) {
      return
    }
    webContents(win).removeListener('context-menu', handleContextMenu)
  }
}
const ContextMenu = () => {
  let isDisposed = false
  const disposables: (() => void)[] = []
  const init = (win: BrowserWindow) => {
    if (isDisposed) {
      return
    }
    const disposeMenu = create(win)
    disposables.push(disposeMenu)
    const removeDisposable = () => {
      const index = disposables.indexOf(disposeMenu)
      if (index !== -1) {
        disposables.splice(index, 1)
      }
    }
    if (typeof win.once !== 'undefined') {
      win.once('closed', removeDisposable)
    }
    disposables.push(() => {
      win.off('closed', removeDisposable)
    })
  }
  const dispose = () => {
    for (const dispose of disposables) {
      dispose()
    }
    disposables.length = 0
    isDisposed = true
  }
  for (const win of electron.BrowserWindow.getAllWindows()) {
    init(win)
  }
  const app = electron.app
  const onWindowCreated = (_event: todo, win: BrowserWindow) => {
    init(win)
  }
  app.on('browser-window-created', onWindowCreated)
  disposables.push(() => {
    app.removeListener('browser-window-created', onWindowCreated)
  })
  return dispose
}
export default ContextMenu