import { getLogger } from '../../../../shared/logger'
import { runtime } from '@deltachat-desktop/runtime-interface'
const log = getLogger('renderer/PlaywrightAutomation')
export class PlaywrightAutomation {
private static instance: PlaywrightAutomation
private initialized: boolean = false
private browser: any = null
private page: any = null
private constructor() {}
public static getInstance(): PlaywrightAutomation {
if (!PlaywrightAutomation.instance) {
PlaywrightAutomation.instance = new PlaywrightAutomation()
}
return PlaywrightAutomation.instance
}
public async initialize(): Promise<boolean> {
if (this.initialized) {
return true
}
try {
const result = await runtime.runCommand(
'npx playwright install chromium && node -e "console.log(\'Playwright is ready\')"'
)
log.info('Playwright initialization result:', result)
this.initialized = true
return true
} catch (error) {
log.error('Failed to initialize Playwright:', error)
return false
}
}
public async searchWeb(query: string): Promise<string> {
if (!this.initialized) {
const success = await this.initialize()
if (!success) {
return "I couldn't access web search capabilities at the moment."
}
}
try {
const searchUrl = `https://duckduckgo.com/?q=${encodeURIComponent(query)}`
const scriptPath = await this.createTempScript(`
const { chromium } = require('playwright');
(async () => {
const browser = await chromium.launch();
const page = await browser.newPage();
await page.goto('${searchUrl}');
await page.waitForSelector('.result__body');
const results = await page.evaluate(() => {
const items = Array.from(document.querySelectorAll('.result__body'));
return items.map(item => {
const title = item.querySelector('.result__title')?.textContent || '';
const snippet = item.querySelector('.result__snippet')?.textContent || '';
return { title, snippet };
}).slice(0, 5);
});
await browser.close();
console.log(JSON.stringify(results));
})();
`)
const output = await runtime.runCommand(`node "${scriptPath}"`)
try {
const results = JSON.parse(output)
return this.formatSearchResults(results)
} catch (e) {
log.error('Failed to parse search results:', e)
return "I found some information, but I couldn't process it properly."
}
} catch (error) {
log.error('Error searching the web:', error)
return 'I encountered an error while trying to search the web.'
}
}
public async captureWebpage(url: string): Promise<string> {
if (!this.initialized) {
const success = await this.initialize()
if (!success) {
throw new Error("Couldn't initialize Playwright")
}
}
try {
const scriptPath = await this.createTempScript(`
const { chromium } = require('playwright');
const path = require('path');
(async () => {
const browser = await chromium.launch();
const page = await browser.newPage();
await page.goto('${url}');
await page.waitForLoadState('networkidle');
const screenshotPath = path.join(process.cwd(), 'webpage_capture.png');
await page.screenshot({ path: screenshotPath, fullPage: true });
await browser.close();
console.log(screenshotPath);
})();
`)
const screenshotPath = await runtime.runCommand(`node "${scriptPath}"`)
return screenshotPath.trim()
} catch (error) {
log.error('Error capturing webpage:', error)
throw new Error('Failed to capture the webpage')
}
}
private async createTempScript(scriptContent: string): Promise<string> {
const filename = `playwright_script_${Date.now()}.js`
const scriptPath = await runtime.writeTempFile(filename, scriptContent)
return scriptPath
}
private formatSearchResults(
results: Array<{ title: string; snippet: string }>
): string {
if (!results || results.length === 0) {
return "I couldn't find any relevant information."
}
let formattedResults = "Here's what I found:\n\n"
results.forEach((result, index) => {
formattedResults += `${index + 1}. ${result.title}\n${result.snippet}\n\n`
})
return formattedResults
}
public isAvailable(): boolean {
return this.initialized
}
}