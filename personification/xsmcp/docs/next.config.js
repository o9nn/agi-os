import { createMDX } from 'fumadocs-mdx/next'
const withMDX = createMDX()
const config = {
  distDir: 'dist',
  images: { unoptimized: true },
  output: 'export',
  reactStrictMode: true,
  serverExternalPackages: ['typescript', 'twoslash'],
}
export default withMDX(config)