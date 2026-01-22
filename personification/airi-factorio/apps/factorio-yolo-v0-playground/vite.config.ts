import path from 'node:path'
import { cwd } from 'node:process'
import Vue from '@vitejs/plugin-vue'
import { LFS, SpaceCard } from 'hfup/vite'
import Unocss from 'unocss/vite'
import { defineConfig } from 'vite'
import VueDevTools from 'vite-plugin-vue-devtools'
export default defineConfig({
  resolve: {
    alias: {
      '~/': `${path.resolve(__dirname, 'src')}/`,
    },
  },
  plugins: [
    Vue({
      include: [/\.vue$/],
      script: {
        defineModel: true,
      },
      features: {
        propsDestructure: true,
      },
    }),
    Unocss(),
    VueDevTools(),
    LFS(),
    SpaceCard({
      root: cwd(),
      title: 'Factorio YOLO v0 Playground',
      emoji: '🎮',
      colorFrom: 'yellow',
      colorTo: 'red',
      sdk: 'static',
      pinned: false,
      license: 'mit',
      models: ['Ultralytics/YOLO11'], 
      short_description: 'The playground for the Factorio YOLO v0 model.',
      thumbnail: 'https://raw.githubusercontent.com/moeru-ai/airi-factorio/refs/heads/main/models/factorio-yolo-v0/assets/thumbnail.jpeg',
    }),
  ],
  assetsInclude: ['**/*.onnx'],
  optimizeDeps: {
    exclude: ['onnxruntime-web'],
  },
  build: {
    sourcemap: true,
  },
})