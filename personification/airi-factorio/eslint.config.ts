import antfu from '@antfu/eslint-config'
export default antfu(
  {
    rules: {
      'ts/naming-convention': 'off',
    },
    yaml: false,
    markdown: false,
  },
  {
    rules: {
      'ts/naming-convention': 'error',
    },
    files: ['***.ts',
      'packages/tstl-plugin-reload-factorio-mod/example*.ts',
      'apps/factorio-yolo-v0-playground/src/workers/vlm-play-worker.ts',
    ],
  },
  {
    rules: {
      'no-console': 'off',
    },
    files: ['packages/vscode-factorio-rcon-evaluator*.ts'],
  },
  {
    ignores: ['models.pixi'],
  },
)