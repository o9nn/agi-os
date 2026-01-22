import { resolve } from 'node:path'
import Vue from '@vitejs/plugin-vue'
import Unocss from 'unocss/vite'
import VueRouter from 'unplugin-vue-router/vite'
import { defineConfig } from 'vite'
export default defineConfig({
plugins: [
VueRouter({
extensions: ['.vue', '.md'],
dts: resolve(import.meta.dirname, 'src', 'typed-router.d.ts'),
}),
Vue(),
Unocss(),
],
})