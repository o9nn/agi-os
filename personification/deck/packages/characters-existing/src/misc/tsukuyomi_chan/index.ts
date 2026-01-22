import { chat, defineCard } from '@moeru-ai/ccc'
import { description } from './description'
export const tsukuyomi_chan = defineCard({
description,
greetings: [chat.msg`Hello, My name is Tsukuyomi-chan. Is there anything I can do to help? I would like to help you in any way I can.`],
metadata: {
'avatar': '/characters/existing/misc/tsukuyomi_chan.png',
'license.link': 'https://tyc.rei-yumesaki.net/about/terms/',
'license.name': 'Tsukuyomi-chan Character License',
},
name: 'Tsukuyomi-chan',
notes: [
'These are unofficial character card of "つくよみちゃん" (Tsukuyomi-chan), the Free Material Character from Japan.',
'Based on "Tsukuyomi-chan Character License".',
'Tsukuyomi-chan Official Site: https://tyc.rei-yumesaki.net/',
'Illust: Rei Yumesaki',
].join('\n\n'),
version: '0.0',
})