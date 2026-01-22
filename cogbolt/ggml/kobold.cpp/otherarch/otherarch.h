#pragma once
#include <cassert>
#include <cinttypes>
#include <cmath>
#include <cstdio>
#include <cstring>
#include <fstream>
#include <iostream>
#include <map>
#include <string>
#include <vector>
#include "utils.h"
#include "model_adapter.h"
struct kcpp_params {
uint32_t seed                 = 0xFFFFFFFF;
int32_t n_predict             =    -1;
int32_t n_ctx                 =     0;
int32_t n_batch               =  2048;
int32_t n_ubatch              =   512;
int      n_threads                   = -1;
int      n_blasthreads               = -1;
int32_t top_k             = 40;
float   top_p             = 0.95f;
float   min_p             = 0.0f;
float   tfs_z             = 1.00f;
float   nsigma            = 0.00f;
float   typical_p         = 1.00f;
float   temp              = 0.80f;
float   smoothing_factor  = 0.00f;
float   repeat_penalty    = 1.10f;
int32_t repeat_last_n     = 64;
float   rep_pen_slope     = 1.0f;
float   presence_penalty  = 0.00f;
int32_t mirostat          = 0;
float   mirostat_tau      = 5.00f;
float   mirostat_eta      = 0.10f;
float   dry_multiplier    = 0.0f;
float   dry_base          = 1.75f;
int32_t dry_allowed_length = 2;
int32_t dry_penalty_last_n = 0;
std::vector<std::string> dry_sequence_breakers;
float xtc_threshold        = 0;
float xtc_probability      = 0;
float   dynatemp_range     = 0.0f;
float   dynatemp_exponent  = 1.0f;
std::string model_filename       = "";
std::string prompt               = "";
bool flash_attn                  = false;
bool use_smartcontext            = false;
bool use_contextshift            = false;
bool use_fastforward             = false;
bool swa_full                    = true;
};
struct gptj_hparams {
int32_t n_vocab = 50400;
int32_t n_ctx   = 2048;
int32_t n_embd  = 4096;
int32_t n_head  = 16;
int32_t n_layer = 28;
int32_t n_rot   = 64;
int32_t ftype   = 1;
float rope_freq_base  = 10000.0f;
float rope_freq_scale = 1.0f;
};
struct gptj_layer {
struct ggml_v3_tensor * ln_1_g;
struct ggml_v3_tensor * ln_1_b;
struct ggml_v3_tensor * c_attn_q_proj_w;
struct ggml_v3_tensor * c_attn_k_proj_w;
struct ggml_v3_tensor * c_attn_v_proj_w;
struct ggml_v3_tensor * c_attn_proj_w;
struct ggml_v3_tensor * c_mlp_fc_w;
struct ggml_v3_tensor * c_mlp_fc_b;
struct ggml_v3_tensor * c_mlp_proj_w;
struct ggml_v3_tensor * c_mlp_proj_b;
};
struct gptj_layer_v2 {
struct ggml_v2_tensor * ln_1_g;
struct ggml_v2_tensor * ln_1_b;
struct ggml_v2_tensor * c_attn_q_proj_w;
struct ggml_v2_tensor * c_attn_k_proj_w;
struct ggml_v2_tensor * c_attn_v_proj_w;
struct ggml_v2_tensor * c_attn_proj_w;
struct ggml_v2_tensor * c_mlp_fc_w;
struct ggml_v2_tensor * c_mlp_fc_b;
struct ggml_v2_tensor * c_mlp_proj_w;
struct ggml_v2_tensor * c_mlp_proj_w_trans;
struct ggml_v2_tensor * c_mlp_proj_b;
};
struct gptj_layer_v1 {
struct ggml_v1_tensor * ln_1_g;
struct ggml_v1_tensor * ln_1_b;
struct ggml_v1_tensor * c_attn_q_proj_w;
struct ggml_v1_tensor * c_attn_k_proj_w;
struct ggml_v1_tensor * c_attn_v_proj_w;
struct ggml_v1_tensor * c_attn_proj_w;
struct ggml_v1_tensor * c_mlp_fc_w;
struct ggml_v1_tensor * c_mlp_fc_b;
struct ggml_v1_tensor * c_mlp_proj_w;
struct ggml_v1_tensor * c_mlp_proj_w_trans;
struct ggml_v1_tensor * c_mlp_proj_b;
};
struct gptj_v1_model {
gptj_hparams hparams;
struct ggml_v1_tensor * ln_f_g;
struct ggml_v1_tensor * ln_f_b;
struct ggml_v1_tensor * wte;
struct ggml_v1_tensor * lmh_g;
struct ggml_v1_tensor * lmh_b;
std::vector<gptj_layer_v1> layers;
struct ggml_v1_tensor * memory_k;
struct ggml_v1_tensor * memory_v;
struct ggml_v1_context * ctx;
std::map<std::string, struct ggml_v1_tensor *> tensors;
};
struct gptj_v2_model {
gptj_hparams hparams;
struct ggml_v2_tensor * ln_f_g;
struct ggml_v2_tensor * ln_f_b;
struct ggml_v2_tensor * wte;
struct ggml_v2_tensor * lmh_g;
struct ggml_v2_tensor * lmh_b;
std::vector<gptj_layer_v2> layers;
struct ggml_v2_tensor * memory_k;
struct ggml_v2_tensor * memory_v;
struct ggml_v2_context * ctx;
std::map<std::string, struct ggml_v2_tensor *> tensors;
};
struct gptj_model {
gptj_hparams hparams;
struct ggml_v3_tensor * ln_f_g;
struct ggml_v3_tensor * ln_f_b;
struct ggml_v3_tensor * wte;
struct ggml_v3_tensor * lmh_g;
struct ggml_v3_tensor * lmh_b;
std::vector<gptj_layer> layers;
struct ggml_v3_tensor * memory_k;
struct ggml_v3_tensor * memory_v;
struct ggml_v3_context * ctx;
std::map<std::string, struct ggml_v3_tensor *> tensors;
};
struct gpt2_hparams {
int32_t n_vocab = 50257;
int32_t n_ctx   = 1024;
int32_t n_embd  = 768;
int32_t n_head  = 12;
int32_t n_layer = 12;
int32_t ftype     = 1;
};
struct gpt2_v1_layer {
struct ggml_v1_tensor * ln_1_g;
struct ggml_v1_tensor * ln_1_b;
struct ggml_v1_tensor * ln_2_g;
struct ggml_v1_tensor * ln_2_b;
struct ggml_v1_tensor * c_attn_attn_w;
struct ggml_v1_tensor * c_attn_attn_b;
struct ggml_v1_tensor * c_attn_proj_w;
struct ggml_v1_tensor * c_attn_proj_b;
struct ggml_v1_tensor * c_mlp_fc_w;
struct ggml_v1_tensor * c_mlp_fc_b;
struct ggml_v1_tensor * c_mlp_proj_w_trans;
struct ggml_v1_tensor * c_mlp_proj_b;
};
struct gpt2_v1_model {
gpt2_hparams hparams;
struct ggml_v1_tensor * ln_f_g;
struct ggml_v1_tensor * ln_f_b;
struct ggml_v1_tensor * wte;
struct ggml_v1_tensor * wpe;
std::vector<gpt2_v1_layer> layers;
struct ggml_v1_tensor * memory_k;
struct ggml_v1_tensor * memory_v;
struct ggml_v1_context * ctx;
std::map<std::string, struct ggml_v1_tensor *> tensors;
};
struct gpt2_layer_v2 {
struct ggml_v2_tensor * ln_1_g;
struct ggml_v2_tensor * ln_1_b;
struct ggml_v2_tensor * ln_2_g;
struct ggml_v2_tensor * ln_2_b;
struct ggml_v2_tensor * c_attn_attn_w;
struct ggml_v2_tensor * c_attn_attn_b;
struct ggml_v2_tensor * c_attn_proj_w;
struct ggml_v2_tensor * c_attn_proj_b;
struct ggml_v2_tensor * c_mlp_fc_w;
struct ggml_v2_tensor * c_mlp_fc_b;
struct ggml_v2_tensor * c_mlp_proj_w;
struct ggml_v2_tensor * c_mlp_proj_b;
};
struct gpt2_v2_model {
gpt2_hparams hparams;
struct ggml_v2_tensor * ln_f_g;
struct ggml_v2_tensor * ln_f_b;
struct ggml_v2_tensor * wte;
struct ggml_v2_tensor * wpe;
struct ggml_v2_tensor * lm_head;
std::vector<gpt2_layer_v2> layers;
struct ggml_v2_tensor * memory_k;
struct ggml_v2_tensor * memory_v;
struct ggml_v2_context * ctx;
std::map<std::string, struct ggml_v2_tensor *> tensors;
};
struct gpt2_layer {
struct ggml_v3_tensor * ln_1_g;
struct ggml_v3_tensor * ln_1_b;
struct ggml_v3_tensor * ln_2_g;
struct ggml_v3_tensor * ln_2_b;
struct ggml_v3_tensor * c_attn_attn_w;
struct ggml_v3_tensor * c_attn_attn_b;
struct ggml_v3_tensor * c_attn_proj_w;
struct ggml_v3_tensor * c_attn_proj_b;
struct ggml_v3_tensor * c_mlp_fc_w;
struct ggml_v3_tensor * c_mlp_fc_b;
struct ggml_v3_tensor * c_mlp_proj_w;
struct ggml_v3_tensor * c_mlp_proj_b;
};
struct gpt2_model {
gpt2_hparams hparams;
struct ggml_v3_tensor * ln_f_g;
struct ggml_v3_tensor * ln_f_b;
struct ggml_v3_tensor * wte;
struct ggml_v3_tensor * wpe;
struct ggml_v3_tensor * lm_head;
std::vector<gpt2_layer> layers;
struct ggml_v3_tensor * memory_k;
struct ggml_v3_tensor * memory_v;
struct ggml_v3_context * ctx;
std::map<std::string, struct ggml_v3_tensor *> tensors;
};
struct gpt_neox_hparams {
int32_t n_vocab = 50257;
int32_t n_ctx   = 4096;
int32_t n_embd  = 4096;
int32_t n_head  = 32;
int32_t n_layer = 16;
int32_t n_rot   = 32;
int32_t par_res = 1;
int32_t ftype   = 1;
float rope_freq_base  = 10000.0f;
float rope_freq_scale = 1.0f;
};
struct gpt_neox_layer_v2 {
struct ggml_v2_tensor * ln_1_g;
struct ggml_v2_tensor * ln_1_b;
struct ggml_v2_tensor * c_attn_attn_w;
struct ggml_v2_tensor * c_attn_attn_b;
struct ggml_v2_tensor * c_attn_proj_w;
struct ggml_v2_tensor * c_attn_proj_b;
struct ggml_v2_tensor * ln_2_g;
struct ggml_v2_tensor * ln_2_b;
struct ggml_v2_tensor * c_mlp_fc_w;
struct ggml_v2_tensor * c_mlp_fc_b;
struct ggml_v2_tensor * c_mlp_proj_w;
struct ggml_v2_tensor * c_mlp_proj_b;
};
struct gpt_neox_v2_model {
gpt_neox_hparams hparams;
struct ggml_v2_tensor * ln_f_g;
struct ggml_v2_tensor * ln_f_b;
struct ggml_v2_tensor * wte;
struct ggml_v2_tensor * lmh_g;
std::vector<gpt_neox_layer_v2> layers;
struct ggml_v2_tensor * memory_k;
struct ggml_v2_tensor * memory_v;
struct ggml_v2_context * ctx;
std::map<std::string, struct ggml_v2_tensor *> tensors;
};
struct gpt_neox_layer {
struct ggml_v3_tensor * ln_1_g;
struct ggml_v3_tensor * ln_1_b;
struct ggml_v3_tensor * c_attn_attn_w;
struct ggml_v3_tensor * c_attn_attn_b;
struct ggml_v3_tensor * c_attn_proj_w;
struct ggml_v3_tensor * c_attn_proj_b;
struct ggml_v3_tensor * ln_2_g;
struct ggml_v3_tensor * ln_2_b;
struct ggml_v3_tensor * c_mlp_fc_w;
struct ggml_v3_tensor * c_mlp_fc_b;
struct ggml_v3_tensor * c_mlp_proj_w;
struct ggml_v3_tensor * c_mlp_proj_b;
};
struct gpt_neox_model {
gpt_neox_hparams hparams;
struct ggml_v3_tensor * ln_f_g;
struct ggml_v3_tensor * ln_f_b;
struct ggml_v3_tensor * wte;
struct ggml_v3_tensor * lmh_g;
std::vector<gpt_neox_layer> layers;
struct ggml_v3_tensor * memory_k;
struct ggml_v3_tensor * memory_v;
struct ggml_v3_context * ctx;
std::map<std::string, struct ggml_v3_tensor *> tensors;
};
struct mpt_hparams {
int32_t d_model      = 0;
int32_t max_seq_len  = 0;
int32_t n_heads      = 0;
int32_t n_layers     = 0;
int32_t n_vocab      = 0;
float alibi_bias_max = 0;
float clip_qkv       = 0;
int32_t ftype        = 0;
int32_t n_ctx        = 0;
};
struct mpt_layer {
struct ggml_v3_tensor * norm_1_weight;
struct ggml_v3_tensor * c_attn_wqkv_weight;
struct ggml_v3_tensor * c_attn_out_proj_weight;
struct ggml_v3_tensor * norm_2_weight;
struct ggml_v3_tensor * ffn_up_proj;
struct ggml_v3_tensor * ffn_down_proj;
};
struct mpt_model {
mpt_hparams hparams;
struct ggml_v3_tensor * wte_weight;
struct ggml_v3_tensor * norm_f_weight;
std::vector<mpt_layer> layers;
struct ggml_v3_tensor * memory_k;
struct ggml_v3_tensor * memory_v;
struct ggml_v3_context * ctx;
std::map<std::string, struct ggml_v3_tensor *> tensors;
};
struct media_chunk
{
int32_t clp_image_tokens = 0;
float * clp_img_embd = nullptr;
};
struct media_object
{
std::string b64data = "";
std::vector<media_chunk> mediachunks;
bool is_audio = false;
std::vector<int> chunk_start_seq;
std::vector<int> chunk_end_seq;
};
struct speculative_draft_result
{
std::vector<int32_t> draftids;
std::vector<float *> actual_logits;
bool draft_success = false;
int drafted_amount = 0;
};
struct savestate_data
{
size_t current_savestate_size = 0;
std::vector<uint8_t> current_savestate_buffer;
size_t current_draft_savestate_size = 0;
std::vector<uint8_t> current_draft_savestate_buffer;
std::vector<gpt_vocab::id> savestate_context_tokens;
};
const float default_norm_eps = 1e-5f;