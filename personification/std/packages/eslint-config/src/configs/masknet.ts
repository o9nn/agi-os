import type { TypedFlatConfigItem } from '@antfu/eslint-config'
import type { MoeruOptions } from '..'
import masknetPlugin from '@masknet/eslint-plugin'
import { GLOB_ASTRO_TS, GLOB_JSX, GLOB_MARKDOWN_CODE, GLOB_SRC_EXT, GLOB_TESTS, GLOB_TS, GLOB_TSX } from '@antfu/eslint-config'
export const masknet = (options: MoeruOptions): TypedFlatConfigItem[] => [
  {
    name: 'moeru/masknet/setup',
    plugins: {
      '@masknet': masknetPlugin,
    },
  },
  {
    files: [GLOB_TS, GLOB_TSX],
    name: 'moeru/masknet/rules',
    rules: {
      '@masknet/array-no-unneeded-flat-map': 'warn',
      '@masknet/browser-no-persistent-storage': 'error',
      '@masknet/browser-no-set-html': 'error',
      '@masknet/browser-prefer-location-assign': 'warn',
      '@masknet/jsx-no-class-component': 'warn',
      '@masknet/jsx-no-logical': 'error',
      '@masknet/jsx-no-set-html': 'error',
      '@masknet/jsx-no-template-literal': 'warn',
      '@masknet/jsx-no-unneeded-nested': 'warn',
      '@masknet/jsx-prefer-test-id': 'error',
      '@masknet/no-builtin-base64': 'error',
      '@masknet/no-for-in': 'warn',
      '@masknet/no-redundant-variable': 'warn',
      '@masknet/no-single-return': 'warn',
      '@masknet/no-top-level': 'error',
      '@masknet/prefer-default-export': 'warn',
      '@masknet/prefer-early-return': 'warn',
      '@masknet/prefer-fetch': 'warn',
      '@masknet/prefer-timer-id': 'error',
      '@masknet/string-no-interpolation': 'warn',
      '@masknet/string-no-locale-case': 'error',
      '@masknet/string-no-simple-template-literal': 'warn',
      '@masknet/type-no-const-enum': 'error',
      '@masknet/type-no-force-cast-via-top-type': 'error',
      '@masknet/type-no-number-constructor': 'error',
      '@masknet/type-no-wrapper-type-reference': 'error',
      '@masknet/unicode-no-bidi': 'error',
      '@masknet/unicode-no-invisible': 'error',
      'unicorn/no-instanceof-array': 'off',
    },
  },
  {
    files: [
      ...GLOB_TESTS,
      GLOB_MARKDOWN_CODE,
      GLOB_JSX,
      GLOB_TSX,
      `**/scripts/*.${GLOB_SRC_EXT}`,
    ],
    rules: {
      '@masknet/no-top-level': 'off',
      '@masknet/unicode-specific-set': 'off',
    },
  },
  ...(typeof options.typescript === 'object' && 'tsconfigPath' in options.typescript
    ? [{
      files: [GLOB_TS, GLOB_TSX],
      ignores: [GLOB_MARKDOWN_CODE, GLOB_ASTRO_TS],
      name: 'moeru/masknet/rules-type-aware',
      rules: {
        '@masknet/no-unsafe-date': 'error',
        '@masknet/string-no-unneeded-to-string': 'warn',
      },
    } satisfies TypedFlatConfigItem]
    : []
  ),
]