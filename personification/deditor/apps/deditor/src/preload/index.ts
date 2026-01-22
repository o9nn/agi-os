import { contextIsolated } from 'node:process'
import { electronAPI } from '@electron-toolkit/preload'
import { contextBridge } from 'electron'
const api = {}
if (contextIsolated) {
  try {
    contextBridge.exposeInMainWorld('electron', electronAPI)
    contextBridge.exposeInMainWorld('api', api)
  }
  catch (error) {
    console.error(error)
  }
}
else {
  window.electron = electronAPI
  window.api = api
}