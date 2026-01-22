import antfu from '@antfu/eslint-config'
export default await antfu(
{
unocss: true,
vue: true,
toml: false,
ignores: [
'buildtsconfig.json'],
rules: {
'jsonc/sort-keys': 'off',
},
},
)