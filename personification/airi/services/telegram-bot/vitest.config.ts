import { cwd } from 'node:process'
import { loadEnv } from 'vite'
import { defineConfig } from 'vitest/config'
export default defineConfig(({ mode }) => {
console.info('mode', mode)
return {
test: {
env: loadEnv(mode, cwd(), ''),
workspace: [
{
extends: true,
test: {
name: 'node',
environment: 'node',
include: ['***.browser.{spec,test}.ts', '**/node_modules/**'],
},
},
],
},
}
})