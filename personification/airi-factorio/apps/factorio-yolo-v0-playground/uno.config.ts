import {
defineConfig,
presetAttributify,
presetIcons,
presetTypography,
presetUno,
transformerDirectives,
transformerVariantGroup,
} from 'unocss'
import presetAnimations from 'unocss-preset-animations'
import { presetShadcn } from 'unocss-preset-shadcn'
export default defineConfig({
shortcuts: [
['btn', 'px-4 py-1 rounded inline-block bg-teal-700 text-white cursor-pointer !outline-none hover:bg-teal-800 disabled:cursor-default disabled:bg-gray-600 disabled:opacity-50'],
['icon-btn', 'inline-block cursor-pointer select-none opacity-75 transition duration-200 ease-in-out hover:opacity-100 hover:text-teal-600'],
{
'animate-accordion-up': 'accordion-up',
'animate-accordion-down': 'accordion-down',
},
],
presets: [
presetUno(),
presetAttributify(),
presetIcons({
scale: 1.2,
}),
presetTypography(),
presetAnimations(),
presetShadcn(),
],
transformers: [
transformerDirectives(),
transformerVariantGroup(),
],
safelist: 'prose prose-sm m-auto text-left'.split(' '),
content: {
pipeline: {
include: [
/\.(vue|svelte|[jt]sx|mdx?|astro|elm|php|phtml|html)($|\?)/,
'src*.{js,ts}',
],
},
},
})