import { docsSchema } from '@astrojs/starlight/schema'
import { glob } from 'astro/loaders'
import { defineCollection, z } from 'astro:content'
export const collections = {
  docs: defineCollection({
    loader: glob({ base: './src/content/docs', pattern: '**/*.{md,mdx}' }),
    schema: docsSchema({
      extend: z.object({
        avatar: z.string().optional(),
        chara_version: z.string().optional(),
        type: z.string().optional(),
      }),
    }),
  }),
}