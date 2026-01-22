import type { PretrainedConfig } from '@huggingface/transformers'
import type { InferenceSession } from 'onnxruntime-common'
import type { ProgressCallback } from './core'
import type { Device } from './devices'
import type { DType } from './dtypes'
export interface ModelSpecificPretrainedOptions {
device?: Device
dtype?: DType
model_file_name?: string
session_options?: InferenceSession.SessionOptions
subfolder?: string
use_external_data_format?: boolean
}
export interface PretrainedOptions {
cache_dir?: string
config?: PretrainedConfig
local_files_only?: boolean
progress_callback?: ProgressCallback
revision?: string
}