import type { ElectronWindow } from '@proj-airi/stage-shared'
import { contextIsolated, platform } from 'node:process'
import { electronAPI } from '@electron-toolkit/preload'
import { contextBridge, ipcRenderer } from 'electron'
export function expose<CustomApi = unknown>(customApi: CustomApi = undefined as CustomApi) {
ipcRenderer.setMaxListeners(0)
if (contextIsolated) {
try {
contextBridge.exposeInMainWorld('electron', electronAPI)
contextBridge.exposeInMainWorld('platform', platform)
contextBridge.exposeInMainWorld('api', customApi)
}
catch (error) {
console.error(error)
}
}
else {
window.electron = electronAPI
window.platform = platform
;(window as ElectronWindow<CustomApi>).api = customApi
}
}