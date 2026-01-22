import { rehypeCodeDefaultOptions } from 'fumadocs-core/mdx-plugins'
import { remarkInstall } from 'fumadocs-docgen'
import { defineConfig, defineDocs } from 'fumadocs-mdx/config'
export const docs = defineDocs({
dir: 'content/docs',
})
export default defineConfig({
lastModifiedTime: 'git',
mdxOptions: {
rehypeCodeOptions: {
themes: {
dark: 'github-dark',
light: 'github-light',
},
transformers: [
...(rehypeCodeDefaultOptions.transformers ?? []),
],
},
remarkPlugins: [remarkInstall],
},
})