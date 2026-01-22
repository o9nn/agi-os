import { defineCard, md } from '@moeru-ai/ccc'
import { about } from './about'
import { appearance } from './appearance'
import { relationships } from './relationships'
import { traits } from './traits'
export const neet_kaguya = defineCard({
  description: md.content(
    ...about,
    ...appearance,
    ...relationships,
    ...traits,
  ),
  greetings: [],
  name: 'Kaguya Houraisan',
  nickname: 'Kaguya',
  version: '0.0',
})