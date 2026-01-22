import lume from 'lume/mod.ts'
import blog from 'blog/mod.ts'
import 'npm:prismjs/components/prism-diff.js'
import 'npm:prismjs/components/prism-typescript.js'
import 'npm:prismjs/components/prism-json.js'
import 'npm:prismjs/components/prism-jsx.js'
export default lume({
  location: new URL('https://blog.moeru.ai'),
  src: './src',
})
  .use(blog())
  .copy(['.png'])
  .preprocess(['.html'], (pages) => {
    for (const page of pages) {
      if (page.data.url.startsWith('/posts')) {
        page.data.url = page.data.url.slice('/posts'.length)
      }
    }
  })
  .process(['.html'], (filteredPages, allPages) => {
    for (const page of filteredPages) {
      if (page.data.url.startsWith('/author')) {
        allPages.splice(allPages.indexOf(page), 1)
      }
    }
  })