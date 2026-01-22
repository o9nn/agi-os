import Vue from '@vitejs/plugin-vue'
import UnoCSS from 'unocss/vite'
import Markdown from 'unplugin-vue-markdown/vite'
import Inspector from 'vite-plugin-inspect'
import { defineConfig } from 'vite'
export default defineConfig({
  plugins: [
    Markdown({}),
    Vue({
      include: ['***.md'],
    }),
    Inspector(),
    UnoCSS(),
  ],
})