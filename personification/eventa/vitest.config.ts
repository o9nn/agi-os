import { defineConfig } from 'vite'
export default defineConfig({
test: {
include: [
'***.spec.ts',
],
setupFiles: ['@vitest/web-worker'],
},
})