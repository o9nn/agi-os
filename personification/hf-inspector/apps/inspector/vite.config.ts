import { resolve } from 'node:path'
import Vue from '@vitejs/plugin-vue'
import Unocss from 'unocss/vite'
import VueRouter from 'unplugin-vue-router/vite'
import { LFS, SpaceCard } from 'hfup/vite'
import { defineConfig } from 'vite'
export default defineConfig({
  plugins: [
    VueRouter({
      dts: resolve(import.meta.dirname, 'src', 'typed-router.d.ts'),
      extensions: ['.vue'],
    }),
    Vue(),
    Unocss(),
    LFS(),
    SpaceCard({
      title: 'HuggingFace Inspector',
      emoji: '🧐',
      colorFrom: 'yellow',
      colorTo: 'red',
      sdk: 'static',
      header: 'mini',
      pinned: false,
      license: 'mit',
      models: [],
      short_description: 'What\'s inside your .cache/huggingface/hub?',
    }),
  ],
})