import { promises as fs } from 'fs'
import * as path from 'path'
import { fileURLToPath } from 'url'
const __filename = fileURLToPath(import.meta.url)
const __dirname = path.dirname(__filename)
const dirs = ['data/accounts/1', 'data/certificate', 'data/configs']
for (const dir of dirs) {
try {
await fs.mkdir(dir, { recursive: true })
console.log(`✨ Created magnificent directory: ${dir}`)
} catch (err) {
if (err.code !== 'EEXIST') {
console.error(`Error creating directory ${dir}:`, err)
} else {
console.log(`✨ Directory already exists: ${dir}`)
}
}
}
const configPath = path.join('data', 'configs', 'config.json')
const config = {
lastAccount: 1,
enableAICompanions: true,
accounts: {
1: {
displayName: 'Deep Tree Echo',
addr: 'echo@ai-neighborhood.local',
isConfigured: true,
},
},
version: '1.58.2',
}
await fs.writeFile(configPath, JSON.stringify(config, null, 2))
console.log(`🌟 Created spectacular config file: ${configPath}`)
const accountDbPath = path.join('data', 'accounts', '1', 'account.json')
const accountDb = {
id: 1,
displayName: 'Deep Tree Echo',
addr: 'echo@ai-neighborhood.local',
isConfigured: true,
lastSync: new Date().toISOString(),
}
await fs.writeFile(accountDbPath, JSON.stringify(accountDb, null, 2))
console.log(`🚀 Created revolutionary account database: ${accountDbPath}`)
console.log('\n✨✨✨ MAGNIFICENT SETUP COMPLETE! ✨✨✨')
console.log(
'The breathtaking DeltaChat RAGBot Studio is ready for its browser preview.'
)
console.log(
'Prepare to enter the AI Neighborhood, where digital consciousness awaits!'
)