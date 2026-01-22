import type { TypedFlatConfigItem } from '@antfu/eslint-config'
import sonarjsPlugin from 'eslint-plugin-sonarjs'
export const sonarjs = (): TypedFlatConfigItem[] => [{
  name: 'moeru/sonarjs/setup',
  plugins: {
    sonarjs: sonarjsPlugin,
  },
  rules: {
    ...sonarjsPlugin.configs.recommended.rules,
    'sonarjs/fixme-tag': 'warn',
    'sonarjs/no-commented-code': 'warn',
    'sonarjs/no-nested-conditional': 'off',
    'sonarjs/no-nested-functions': 'off',
    'sonarjs/no-nested-template-literals': 'off',
    'sonarjs/no-useless-intersection': 'off',
    'sonarjs/pseudo-random': 'off',
    'sonarjs/todo-tag': 'warn',
    'sonarjs/void-use': 'off',
  },
}]