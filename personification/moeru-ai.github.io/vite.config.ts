import unocss from '@unocss/vite'
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'
export default defineConfig({
build: {
target: 'esnext',
},
optimizeDeps: {
esbuildOptions: {
target: 'esnext',
},
},
plugins: [
react({ babel: { plugins: [['babel-plugin-react-compiler', { target: '19' }]] } }),
unocss(),
],
})