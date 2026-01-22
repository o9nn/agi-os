import { defineConfig, devices } from '@playwright/test'
const port = process.env.PORT ?? 3000
const baseURL = `https://localhost:${port}`
export default defineConfig({
testDir: './tests',
fullyParallel: true,
forbidOnly: !!process.env.CI,
retries: 0,
workers: 1,
reporter: [['list'], ['html']],
expect: {
timeout: 20_000,
},
use: {
baseURL: baseURL,
trace: 'on-first-retry',
video: 'retain-on-failure',
screenshot: 'only-on-failure',
permissions: ['notifications'],
ignoreHTTPSErrors: true,
launchOptions: {
args: ['--ignore-certificate-errors'],
},
},
projects: [
{
name: 'Chrome',
use: {
...devices['Desktop Chrome'],
},
},
],
webServer: {
command: `node ${
process.env.CI ? '' : '--env-file .env'
} ../target-browser/dist/server.js`,
url: baseURL,
timeout: 120 * 1000,
ignoreHTTPSErrors: true,
reuseExistingServer: !process.env.CI,
stdout: 'pipe',
stderr: 'pipe',
},
})