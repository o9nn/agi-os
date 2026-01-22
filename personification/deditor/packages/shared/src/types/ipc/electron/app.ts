export interface AppMethods {
getPath: (params: {
name:
| 'home'
| 'appData'
| 'userData'
| 'sessionData'
| 'temp'
| 'exe'
| 'module'
| 'desktop'
| 'documents'
| 'downloads'
| 'music'
| 'pictures'
| 'videos'
| 'recent'
| 'logs'
| 'crashDumps'
}) => string
}