import { rehypeCodeDefaultOptions } from 'fumadocs-core/mdx-plugins'
import {
defineConfig,
defineDocs,
frontmatterSchema,
metaSchema,
} from 'fumadocs-mdx/config'
import { transformerTwoslash } from 'fumadocs-twoslash'
import { createFileSystemTypesCache } from 'fumadocs-twoslash/cache-fs'
export const docs = defineDocs({
docs: {
schema: frontmatterSchema,
},
meta: {
schema: metaSchema,
},
})
export default defineConfig({
mdxOptions: {
rehypeCodeOptions: {
langs: ['bash', 'js', 'jsx', 'ts', 'tsx'],
themes: {
dark: 'github-dark',
light: 'github-light',
},
transformers: [
...(rehypeCodeDefaultOptions.transformers ?? []),
transformerTwoslash({
typesCache: createFileSystemTypesCache(),
}),
],
},
},
})