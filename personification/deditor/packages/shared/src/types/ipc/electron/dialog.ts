export interface DialogMethods {
showOpenDialog: (params: {
title?: string
defaultPath?: string
buttonLabel?: string
filters?: {
extensions: string[]
name: string
}[]
properties?: Array<'openFile' | 'openDirectory' | 'multiSelections' | 'showHiddenFiles' | 'createDirectory' | 'promptToCreate' | 'noResolveAliases' | 'treatPackageAsDirectory' | 'dontAddToRecent'>
message?: string
securityScopedBookmarks?: boolean
}) => {
canceled: boolean
filePaths: string[]
bookmarks?: string[]
}
}