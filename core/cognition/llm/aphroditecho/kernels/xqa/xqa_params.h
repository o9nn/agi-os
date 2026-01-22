#pragma once
#include "decoder_xqa_common.h"
using XQADataType = Data_type;
struct XQAParams {
XQADataType data_type = DATA_TYPE_FP16;
XQADataType kv_cache_data_type = DATA_TYPE_FP16;
void* output = nullptr;
void const* qHeads = nullptr;
float const* kv_scale_quant_orig = nullptr;
uint32_t* semaphores = nullptr;
void* workspaces = nullptr;
uint32_t batch_size = 0;
int32_t beam_width = 0;
int32_t generation_input_length;
int32_t layer_idx = 0;
int32_t num_q_heads = 0;
int32_t num_kv_heads = 0;
int32_t head_size = 0;
int timestep = 0;
bool paged_kv_cache = true;
int tokens_per_block;
int max_blocks_per_sequence;
bool multi_block_mode;
bool multi_query_tokens = false;
};