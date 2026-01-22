import type { Config } from 'tailwindcss'
import catppuccin from '@catppuccin/daisyui'
import daisyui, { type CustomTheme } from 'daisyui'
const renameTheme = (theme: CustomTheme, name: string) => Object.fromEntries(Object.entries(theme)
  .map(([, value]) => [name, value]))
export default {
  content: ['./src*.{astro,html,js,jsx,md,mdx,svelte,ts,tsx,vue}'],
  corePlugins: { preflight: false },
  daisyui: {
    logs: false,
    themes: [
      renameTheme(catppuccin('latte', 'teal'), 'light'), 
      renameTheme(catppuccin('macchiato', 'green'), 'dark'), 
    ],
  },
  plugins: [daisyui],
} satisfies Config