import unocss from '@unocss/vite'
import react from '@vitejs/plugin-react'
// import { fontless } from 'fontless'
import { defineConfig } from 'vite'

// https://vitejs.dev/config/
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
    // https://github.com/unjs/fontaine/tree/main/packages/fontless#configuration
    // fontless(),
    unocss(),
  ],
})
