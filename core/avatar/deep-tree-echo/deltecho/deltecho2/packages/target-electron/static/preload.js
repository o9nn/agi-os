;(() => {
  const electron = require('electron')
  let is_first_time = true
  window.get_electron_functions = () => {
    if (!is_first_time) {
      console.warn(
        'tried to get get_electron_functions multiple times, this is not expected'
      )
      throw new Error(
        'get_electron_functions was accessed previously, it can only be accessed one time'
      )
    }
    is_first_time = false
    delete window.get_electron_functions
    return {
      ipcRenderer: electron.ipcRenderer,
      app_getPath: p => electron.ipcRenderer.sendSync('app-get-path', p),
      getPathForFile: electron.webUtils.getPathForFile,
    }
  }
  console.log({ global })
})()