#pragma once
#include "llama.h"
#include <cstdint>
struct llama_cparams {
uint32_t n_ctx;
uint32_t n_batch;
uint32_t n_ubatch;
uint32_t n_seq_max;
int n_threads;
int n_threads_batch;
float rope_freq_base;
float rope_freq_scale;
uint32_t n_ctx_orig_yarn;
float yarn_ext_factor;
float yarn_attn_factor;
float yarn_beta_fast;
float yarn_beta_slow;
float defrag_thold;
bool embeddings;
bool causal_attn;
bool offload_kqv;
bool flash_attn;
bool no_perf;
bool warmup;
bool op_offload;
enum llama_pooling_type pooling_type;
ggml_backend_sched_eval_callback cb_eval;
void * cb_eval_user_data;
};