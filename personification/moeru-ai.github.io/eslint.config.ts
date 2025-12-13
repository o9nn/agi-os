import { defineConfig } from '@importantimport/eslint-config'
import reactCompiler from 'eslint-plugin-react-compiler'

export default defineConfig({
  react: true,
  typescript: { tsconfigPath: './tsconfig.json' },
}, [
  { ignores: ['cspell.config.yaml'] },
  reactCompiler.configs.recommended,
])
