import { presetWebFonts } from '@unocss/preset-web-fonts'
import { presetWind4 } from '@unocss/preset-wind4'
import { defineConfig } from '@unocss/vite'
export default defineConfig({
  presets: [
    presetWind4({
      preflights: {
        reset: true,
      },
    }),
    presetWebFonts({
      fonts: {
        doto: [
          {
            name: 'Doto Variable',
            provider: 'none',
          },
          {
            name: 'sans-serif',
            provider: 'none',
          },
        ],
        pixel: [
          {
            name: 'Fusion Pixel 10px Monospaced SC',
            provider: 'fontsource',
          },
          {
            name: 'sans-serif',
            provider: 'none',
          },
        ],
      },
      themeKey: 'font',
    }),
  ],
})