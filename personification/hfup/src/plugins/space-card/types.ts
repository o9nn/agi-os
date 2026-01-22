export const colorFromValues: string[] = ['red', 'yellow', 'green', 'blue', 'indigo', 'purple', 'pink', 'gray']
export const colorToValues: string[] = ['red', 'yellow', 'green', 'blue', 'indigo', 'purple', 'pink', 'gray']
export const sdkValues: string[] = ['gradio', 'streamlit', 'docker', 'static']
export const suggestedStorageValues: string[] = ['small', 'medium', 'large']
export const headerValues: string[] = ['mini', 'default']
export const licenseValues: string[] = [
'apache-2.0',
'mit',
'openrail',
'bigscience-openrail-m',
'creativeml-openrail-m',
'bigscience-bloom-rail-1.0',
'bigcode-openrail-m',
'afl-3.0',
'artistic-2.0',
'bsl-1.0',
'bsd',
'bsd-2-clause',
'bsd-3-clause',
'bsd-3-clause-clear',
'c-uda',
'cc',
'cc0-1.0',
'cc-by-2.0',
'cc-by-2.5',
'cc-by-3.0',
'cc-by-4.0',
'cc-by-sa-3.0',
'cc-by-sa-4.0',
'cc-by-nc-2.0',
'cc-by-nc-3.0',
'cc-by-nc-4.0',
'cc-by-nd-4.0',
'cc-by-nc-nd-3.0',
'cc-by-nc-nd-4.0',
'cc-by-nc-sa-2.0',
'cc-by-nc-sa-3.0',
'cc-by-nc-sa-4.0',
'cdla-sharing-1.0',
'cdla-permissive-1.0',
'cdla-permissive-2.0',
'wtfpl',
'ecl-2.0',
'epl-1.0',
'epl-2.0',
'etalab-2.0',
'eupl-1.1',
'agpl-3.0',
'gfdl',
'gpl',
'gpl-2.0',
'gpl-3.0',
'lgpl',
'lgpl-2.1',
'lgpl-3.0',
'isc',
'lppl-1.3c',
'ms-pl',
'apple-ascl',
'mpl-2.0',
'odc-by',
'odbl',
'openrail++',
'osl-3.0',
'postgresql',
'ofl-1.1',
'ncsa',
'unlicense',
'zlib',
'pddl',
'lgpl-lr',
'deepfloyd-if-license',
'llama2',
'llama3',
'llama3.1',
'llama3.2',
'llama3.3',
'gemma',
'unknown',
'other',
]
export type License = 'apache-2.0' | 'mit' | 'openrail' | 'bigscience-openrail-m' | 'creativeml-openrail-m'
| 'bigscience-bloom-rail-1.0' | 'bigcode-openrail-m' | 'afl-3.0' | 'artistic-2.0' | 'bsl-1.0' | 'bsd'
| 'bsd-2-clause' | 'bsd-3-clause' | 'bsd-3-clause-clear' | 'c-uda' | 'cc' | 'cc0-1.0' | 'cc-by-2.0'
| 'cc-by-2.5' | 'cc-by-3.0' | 'cc-by-4.0' | 'cc-by-sa-3.0' | 'cc-by-sa-4.0' | 'cc-by-nc-2.0'
| 'cc-by-nc-3.0' | 'cc-by-nc-4.0' | 'cc-by-nd-4.0' | 'cc-by-nc-nd-3.0' | 'cc-by-nc-nd-4.0'
| 'cc-by-nc-sa-2.0' | 'cc-by-nc-sa-3.0' | 'cc-by-nc-sa-4.0' | 'cdla-sharing-1.0' | 'cdla-permissive-1.0'
| 'cdla-permissive-2.0' | 'wtfpl' | 'ecl-2.0' | 'epl-1.0' | 'epl-2.0' | 'etalab-2.0' | 'eupl-1.1'
| 'agpl-3.0' | 'gfdl' | 'gpl' | 'gpl-2.0' | 'gpl-3.0' | 'lgpl' | 'lgpl-2.1' | 'lgpl-3.0' | 'isc'
| 'lppl-1.3c' | 'ms-pl' | 'apple-ascl' | 'mpl-2.0' | 'odc-by' | 'odbl' | 'openrail++' | 'osl-3.0'
| 'postgresql' | 'ofl-1.1' | 'ncsa' | 'unlicense' | 'zlib' | 'pddl' | 'lgpl-lr' | 'deepfloyd-if-license'
| 'llama2' | 'llama3' | 'llama3.1' | 'llama3.2' | 'llama3.3' | 'gemma' | 'unknown' | 'other'
export interface SpaceConfiguration {
title?: string
license?: License | License[]
emoji?: string
colorFrom?: 'red' | 'yellow' | 'green' | 'blue' | 'indigo' | 'purple' | 'pink' | 'gray'
colorTo?: 'red' | 'yellow' | 'green' | 'blue' | 'indigo' | 'purple' | 'pink' | 'gray'
sdk?: 'gradio' | 'streamlit' | 'docker' | 'static'
python_version?: string
sdk_version?: string
suggested_hardware?: string
suggested_storage?: 'small' | 'medium' | 'large'
app_file?: string
app_port?: number
base_path?: string
fullWidth?: boolean
header?: 'mini' | 'default'
short_description?: string
models?: string[]
datasets?: string[]
tags?: string[]
thumbnail?: string
pinned?: boolean
hf_oauth?: boolean
hf_oauth_scopes?: string[]
hf_oauth_expiration_minutes?: number
disable_embedding?: boolean
startup_duration_timeout?: string
custom_headers?: Record<string, string>
preload_from_hub?: string[]
}