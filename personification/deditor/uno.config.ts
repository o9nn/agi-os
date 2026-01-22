import type { PresetOrFactoryAwaitable } from 'unocss'
import presetAnimations from 'unocss-preset-animations'
import { createExternalPackageIconLoader } from '@iconify/utils/lib/loader/external-pkg'
import { presetChromatic } from '@proj-airi/unocss-preset-chromatic'
import { colorToString } from '@unocss/preset-mini/utils'
import { createLocalFontProcessor } from '@unocss/preset-web-fonts/local'
import { defineConfig, mergeConfigs, presetAttributify, presetIcons, presetTypography, presetWebFonts, presetWind3, transformerDirectives, transformerVariantGroup } from 'unocss'
import { presetScrollbar } from 'unocss-preset-scrollbar'
import { presetShadcn } from 'unocss-preset-shadcn'
import { parseColor } from 'unocss/preset-mini'
export function presetStoryMockHover(): PresetOrFactoryAwaitable {
return {
name: 'story-mock-hover',
variants: [
(matcher) => {
if (!matcher.includes('hover')) {
return matcher
}
return {
matcher,
selector: (s) => {
return `${s}, ${s.replace(/:hover$/, '')}._hover`
},
}
},
],
}
}
export function safelistAllPrimaryBackgrounds(): string[] {
return [
...[undefined, 50, 100, 200, 300, 400, 500, 600, 700, 800, 900, 950].map((shade) => {
const prefix = shade ? `bg-primary-${shade}` : `bg-primary`
return [
prefix,
...[5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100].map(opacity => `${prefix}/${opacity}`),
]
}).flat(),
]
}
export function sharedUnoConfig() {
return defineConfig({
presets: [
presetWind3(),
presetAttributify(),
presetTypography(),
presetWebFonts({
fonts: {
sans: {
name: 'DM Sans',
weights: [
100,
200,
300,
400,
500,
600,
700,
800,
900,
],
},
serif: 'DM Serif Display',
mono: 'DM Mono',
cute: 'Kiwi Maru',
cuteen: 'Sniglet',
jura: 'Jura',
gugi: 'Gugi',
quicksand: 'Quicksand',
quanlai: {
name: 'cjkfonts AllSeto',
provider: 'none',
},
xiaolai: {
name: 'Xiaolai SC',
provider: 'none',
},
},
timeouts: {
warning: 5000,
failure: 10000,
},
processors: createLocalFontProcessor({
cacheDir: 'node_modules/.cache/unocss/fonts',
fontAssetsDir: 'src/renderer/public/assets/fonts',
fontServeBaseUrl: '/assets/fonts',
}),
}),
presetIcons({
scale: 1.2,
collections: {
...createExternalPackageIconLoader('@deditor-app/deditor-icons'),
...createExternalPackageIconLoader('@deditor-app/drizzle-orm-icons'),
},
}),
presetScrollbar(),
presetAnimations(),
presetShadcn(
{ color: 'neutral' },
{ componentLibrary: 'reka' },
),
presetChromatic({
baseHue: 295.44,
colors: {
primary: 0,
complementary: 180,
},
}),
],
transformers: [
transformerDirectives({
applyVariable: ['--at-apply'],
}),
transformerVariantGroup(),
],
safelist: [
...'prose prose-sm m-auto text-left'.split(' '),
],
content: {
pipeline: {
include: [
/\.(vue|svelte|[jt]sx|mdx?|astro|elm|php|phtml|html)($|\?)/,
'(components|src)*.{js,ts}',
],
},
},
rules: [
[/^mask-\[(.*)\]$/, ([, suffix]) => ({ '-webkit-mask-image': suffix.replace(/_/g, ' ') })],
[/^bg-dotted-\[(.*)\]$/, ([, color], { theme }) => {
const parsedColor = parseColor(color, theme)
return {
'background-image': `radial-gradient(circle at 1px 1px, ${colorToString(parsedColor?.cssColor ?? parsedColor?.color ?? color, 'var(--un-background-opacity)')} 1px, transparent 0)`,
'--un-background-opacity': parsedColor?.cssColor?.alpha ?? parsedColor?.alpha ?? 1,
}
}],
],
theme: {
animation: {
keyframes: {
slideUpAndFade: '{from{opacity:0;transform:translateY(2px)}to{opacity:1;transform:translateY(0)}}',
slideRightAndFade: '{from{opacity:0;transform:translateX(-2px)}to{opacity:1;transform:translateX(0)}}',
slideDownAndFade: '{from{opacity:0;transform:translateY(-2px)}to{opacity:1;transform:translateY(0)}}',
slideLeftAndFade: '{from{opacity:0;transform:translateX(2px)}to{opacity:1;transform:translateX(0)}}',
},
durations: {
slideUpAndFade: '400ms',
slideRightAndFade: '400ms',
slideDownAndFade: '400ms',
slideLeftAndFade: '400ms',
},
timingFns: {
slideUpAndFade: 'cubic-bezier(0.16, 1, 0.3, 1)',
slideRightAndFade: 'cubic-bezier(0.16, 1, 0.3, 1)',
slideDownAndFade: 'cubic-bezier(0.16, 1, 0.3, 1)',
slideLeftAndFade: 'cubic-bezier(0.16, 1, 0.3, 1)',
},
},
},
})
}
export default mergeConfigs([
sharedUnoConfig(),
])