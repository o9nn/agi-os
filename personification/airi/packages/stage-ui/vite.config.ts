import type { Plugin } from 'vite'
import { join, resolve } from 'node:path'
import Vue from '@vitejs/plugin-vue'
import Unocss from 'unocss/vite'
import Yaml from 'unplugin-yaml/vite'
import Inspect from 'vite-plugin-inspect'
import { defineConfig } from 'vite'
export default defineConfig({
  resolve: {
    alias: {
      '@proj-airi/i18n': resolve(join(import.meta.dirname, '..', '..', 'packages', 'i18n', 'src')),
      '@proj-airi/stage-shared': resolve(join(import.meta.dirname, '..', '..', 'packages', 'stage-shared', 'src')),
    },
  },
  server: {
    fs: {
      allow: [join('..', '..')],
    },
  },
  optimizeDeps: {
    include: [
      '@shikijs/rehype',
      'rehype-stringify',
      'remark-math',
      'remark-parse',
      'unified',
      'flexsearch',
      'shiki',
      'vscode-oniguruma',
      'vscode-textmate',
    ],
  },
  plugins: [
    Yaml() as Plugin,
    Vue(),
    Unocss(),
    Inspect() as Plugin,
  ],
})