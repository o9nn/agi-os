#ifndef LLAMA_V3_H
#define LLAMA_V3_H
#include "ggml_v3.h"
#ifdef GGML_USE_CUDA
#include "ggml_v3-cuda.h"
#define LLAMA_V3_MAX_DEVICES GGML_V3_CUDA_MAX_DEVICES
#else
#define LLAMA_V3_MAX_DEVICES 1
#endif
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef LLAMA_V3_SHARED
#    if defined(_WIN32) && !defined(__MINGW32__)
#        ifdef LLAMA_V3_BUILD
#            define LLAMA_V3_API __declspec(dllexport)
#        else
#            define LLAMA_V3_API __declspec(dllimport)
#        endif
#    else
#        define LLAMA_V3_API __attribute__ ((visibility ("default")))
#    endif
#else
#    define LLAMA_V3_API
#endif
#ifdef __GNUC__
#    define DEPRECATED(func, hint) func __attribute__((deprecated(hint)))
#elif defined(_MSC_VER)
#    define DEPRECATED(func, hint) __declspec(deprecated(hint)) func
#else
#    define DEPRECATED(func, hint) func
#endif
#define LLAMA_V3_FILE_MAGIC_GGJT        0x67676a74u
#define LLAMA_V3_FILE_MAGIC_GGLA        0x67676c61u
#define LLAMA_V3_FILE_MAGIC_GGMF        0x67676d66u
#define LLAMA_V3_FILE_MAGIC_GGML        0x67676d6cu
#define LLAMA_V3_FILE_MAGIC_GGSN        0x6767736eu
#define LLAMA_V3_FILE_VERSION           3
#define LLAMA_V3_FILE_MAGIC             LLAMA_V3_FILE_MAGIC_GGJT
#define LLAMA_V3_FILE_MAGIC_UNVERSIONED LLAMA_V3_FILE_MAGIC_GGML
#define LLAMA_V3_SESSION_MAGIC          LLAMA_V3_FILE_MAGIC_GGSN
#define LLAMA_V3_SESSION_VERSION        1
#define LLAMA_V3_DEFAULT_SEED           0xFFFFFFFF
#if defined(GGML_USE_CUDA) || defined(GGML_USE_CLBLAST) || defined(GGML_USE_METAL)
#define LLAMA_V3_SUPPORTS_GPU_OFFLOAD
#endif
#ifndef LLAMA_V3_DEFAULT_RMS_EPS
#define LLAMA_V3_DEFAULT_RMS_EPS 5e-6f
#endif
#ifdef __cplusplus
extern "C" {
#endif
struct llama_v3_model;
struct llama_v3_context;
typedef int llama_v3_token;
typedef struct llama_v3_token_data {
llama_v3_token id;
float logit;
float p;
} llama_v3_token_data;
typedef struct llama_v3_token_data_array {
llama_v3_token_data * data;
size_t size;
bool sorted;
} llama_v3_token_data_array;
typedef void (*llama_v3_progress_callback)(float progress, void *ctx);
enum llama_v3_log_level {
LLAMA_V3_LOG_LEVEL_ERROR = 2,
LLAMA_V3_LOG_LEVEL_WARN  = 3,
LLAMA_V3_LOG_LEVEL_INFO  = 4
};
typedef void (*llama_v3_log_callback)(enum llama_v3_log_level level, const char * text, void * user_data);
struct llama_v3_context_params {
uint32_t seed;
int32_t  n_ctx;
int32_t  n_batch;
int32_t  n_gqa;
float    rms_norm_eps;
int32_t  n_gpu_layers;
int32_t  main_gpu;
const float * tensor_split;
float    rope_freq_base;
float    rope_freq_scale;
llama_v3_progress_callback progress_callback;
void * progress_callback_user_data;
bool low_vram;
bool mul_mat_q;
bool f16_kv;
bool logits_all;
bool vocab_only;
bool use_mmap;
bool use_mlock;
bool embedding;
};
enum llama_v3_ftype {
LLAMA_V3_FTYPE_ALL_F32              = 0,
LLAMA_V3_FTYPE_MOSTLY_F16           = 1,
LLAMA_V3_FTYPE_MOSTLY_Q4_0          = 2,
LLAMA_V3_FTYPE_MOSTLY_Q4_1          = 3,
LLAMA_V3_FTYPE_MOSTLY_Q4_1_SOME_F16 = 4,
LLAMA_V3_FTYPE_MOSTLY_Q8_0          = 7,
LLAMA_V3_FTYPE_MOSTLY_Q5_0          = 8,
LLAMA_V3_FTYPE_MOSTLY_Q5_1          = 9,
LLAMA_V3_FTYPE_MOSTLY_Q2_K          = 10,
LLAMA_V3_FTYPE_MOSTLY_Q3_K_S        = 11,
LLAMA_V3_FTYPE_MOSTLY_Q3_K_M        = 12,
LLAMA_V3_FTYPE_MOSTLY_Q3_K_L        = 13,
LLAMA_V3_FTYPE_MOSTLY_Q4_K_S        = 14,
LLAMA_V3_FTYPE_MOSTLY_Q4_K_M        = 15,
LLAMA_V3_FTYPE_MOSTLY_Q5_K_S        = 16,
LLAMA_V3_FTYPE_MOSTLY_Q5_K_M        = 17,
LLAMA_V3_FTYPE_MOSTLY_Q6_K          = 18,
};
typedef struct llama_v3_model_quantize_params {
int nthread;
enum llama_v3_ftype   ftype;
bool allow_requantize;
bool quantize_output_tensor;
} llama_v3_model_quantize_params;
struct llama_v3_grammar;
enum llama_v3_gretype {
LLAMA_V3_GRETYPE_END            = 0,
LLAMA_V3_GRETYPE_ALT            = 1,
LLAMA_V3_GRETYPE_RULE_REF       = 2,
LLAMA_V3_GRETYPE_CHAR           = 3,
LLAMA_V3_GRETYPE_CHAR_NOT       = 4,
LLAMA_V3_GRETYPE_CHAR_RNG_UPPER = 5,
LLAMA_V3_GRETYPE_CHAR_ALT       = 6,
};
typedef struct llama_v3_grammar_element {
enum llama_v3_gretype type;
uint32_t           value;
} llama_v3_grammar_element;
struct llama_v3_timings {
double t_start_ms;
double t_end_ms;
double t_load_ms;
double t_sample_ms;
double t_p_eval_ms;
double t_eval_ms;
int32_t n_sample;
int32_t n_p_eval;
int32_t n_eval;
};
LLAMA_V3_API void llama_v3_log_set(llama_v3_log_callback log_callback, void * user_data);
LLAMA_V3_API int llama_v3_max_devices();
LLAMA_V3_API struct llama_v3_context_params llama_v3_context_default_params();
LLAMA_V3_API struct llama_v3_model_quantize_params llama_v3_model_quantize_default_params();
LLAMA_V3_API bool llama_v3_mmap_supported();
LLAMA_V3_API bool llama_v3_mlock_supported();
LLAMA_V3_API void llama_v3_backend_init(bool numa);
LLAMA_V3_API void llama_v3_backend_free();
LLAMA_V3_API int64_t llama_v3_time_us();
LLAMA_V3_API struct llama_v3_model * llama_v3_load_model_from_file(
const char * path_model,
struct llama_v3_context_params   params);
LLAMA_V3_API void llama_v3_free_model(struct llama_v3_model * model);
LLAMA_V3_API struct llama_v3_context * llama_v3_new_context_with_model(
struct llama_v3_model * model,
struct llama_v3_context_params   params);
LLAMA_V3_API struct llama_v3_context * llama_v3_init_from_file(
const char * path_model,
struct llama_v3_context_params   params);
LLAMA_V3_API void llama_v3_free(struct llama_v3_context * ctx);
LLAMA_V3_API int llama_v3_model_quantize(
const char * fname_inp,
const char * fname_out,
const llama_v3_model_quantize_params * params);
LLAMA_V3_API int llama_v3_apply_lora_from_file(
struct llama_v3_context * ctx,
const char * path_lora,
const char * path_base_model,
int   n_threads);
LLAMA_V3_API int llama_v3_model_apply_lora_from_file(
const struct llama_v3_model * model,
const char * path_lora,
const char * path_base_model,
int   n_threads);
LLAMA_V3_API int llama_v3_get_kv_cache_token_count(const struct llama_v3_context * ctx);
LLAMA_V3_API void llama_v3_set_rng_seed(struct llama_v3_context * ctx, uint32_t seed);
LLAMA_V3_API size_t llama_v3_get_state_size(const struct llama_v3_context * ctx);
LLAMA_V3_API size_t llama_v3_copy_state_data(struct llama_v3_context * ctx, uint8_t * dst);
LLAMA_V3_API size_t llama_v3_set_state_data(struct llama_v3_context * ctx, uint8_t * src);
LLAMA_V3_API bool llama_v3_load_session_file(struct llama_v3_context * ctx, const char * path_session, llama_v3_token * tokens_out, size_t n_token_capacity, size_t * n_token_count_out);
LLAMA_V3_API bool llama_v3_save_session_file(struct llama_v3_context * ctx, const char * path_session, const llama_v3_token * tokens, size_t n_token_count);
LLAMA_V3_API int llama_v3_eval(
struct llama_v3_context * ctx,
const llama_v3_token * tokens,
int   n_tokens,
int   n_past,
int   n_threads);
LLAMA_V3_API int llama_v3_eval_embd(
struct llama_v3_context * ctx,
const float * embd,
int   n_tokens,
int   n_past,
int   n_threads);
LLAMA_V3_API int llama_v3_eval_export(struct llama_v3_context * ctx, const char * fname);
LLAMA_V3_API int llama_v3_tokenize(
struct llama_v3_context * ctx,
const char * text,
llama_v3_token * tokens,
int   n_max_tokens,
bool   add_bos);
LLAMA_V3_API int llama_v3_tokenize_with_model(
const struct llama_v3_model * model,
const char * text,
llama_v3_token * tokens,
int   n_max_tokens,
bool   add_bos);
LLAMA_V3_API int llama_v3_n_vocab(const struct llama_v3_context * ctx);
LLAMA_V3_API int llama_v3_n_ctx  (const struct llama_v3_context * ctx);
LLAMA_V3_API int llama_v3_n_embd (const struct llama_v3_context * ctx);
LLAMA_V3_API int llama_v3_n_vocab_from_model(const struct llama_v3_model * model);
LLAMA_V3_API int llama_v3_n_ctx_from_model  (const struct llama_v3_model * model);
LLAMA_V3_API int llama_v3_n_embd_from_model (const struct llama_v3_model * model);
LLAMA_V3_API int llama_v3_model_type(const struct llama_v3_model * model, char * buf, size_t buf_size);
LLAMA_V3_API int llama_v3_get_vocab(
const struct llama_v3_context * ctx,
const char * * strings,
float * scores,
int   capacity);
LLAMA_V3_API int llama_v3_get_vocab_from_model(
const struct llama_v3_model * model,
const char * * strings,
float * scores,
int   capacity);
LLAMA_V3_API float * llama_v3_get_logits(struct llama_v3_context * ctx);
LLAMA_V3_API float * llama_v3_get_embeddings(struct llama_v3_context * ctx);
LLAMA_V3_API const char * llama_v3_token_to_str(
const struct llama_v3_context * ctx,
llama_v3_token   token);
LLAMA_V3_API const char * llama_v3_token_to_str_with_model(
const struct llama_v3_model * model,
llama_v3_token   token);
LLAMA_V3_API llama_v3_token llama_v3_token_bos();
LLAMA_V3_API llama_v3_token llama_v3_token_eos();
LLAMA_V3_API llama_v3_token llama_v3_token_nl();
LLAMA_V3_API struct llama_v3_grammar * llama_v3_grammar_init(
const llama_v3_grammar_element ** rules,
size_t    n_rules,
size_t    start_rule_index);
LLAMA_V3_API void llama_v3_grammar_free(struct llama_v3_grammar * grammar);
LLAMA_V3_API void llama_v3_sample_repetition_penalty(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, const llama_v3_token * last_tokens, size_t last_tokens_size, float penalty);
LLAMA_V3_API void llama_v3_sample_frequency_and_presence_penalties(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, const llama_v3_token * last_tokens, size_t last_tokens_size, float alpha_frequency, float alpha_presence);
LLAMA_V3_API void llama_v3_sample_classifier_free_guidance(
struct llama_v3_context * ctx,
llama_v3_token_data_array * candidates,
struct llama_v3_context * guidance_ctx,
float   scale);
LLAMA_V3_API void llama_v3_sample_softmax(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates);
LLAMA_V3_API void llama_v3_sample_top_k(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, int k, size_t min_keep);
LLAMA_V3_API void llama_v3_sample_top_p(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float p, size_t min_keep);
LLAMA_V3_API void llama_v3_sample_tail_free(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float z, size_t min_keep);
LLAMA_V3_API void llama_v3_sample_typical(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float p, size_t min_keep);
LLAMA_V3_API void llama_v3_sample_temperature(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float temp);
LLAMA_V3_API void llama_v3_sample_grammar(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, const struct llama_v3_grammar * grammar);
LLAMA_V3_API llama_v3_token llama_v3_sample_token_mirostat(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float tau, float eta, int m, float * mu);
LLAMA_V3_API llama_v3_token llama_v3_sample_token_mirostat_v2(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates, float tau, float eta, float * mu);
LLAMA_V3_API llama_v3_token llama_v3_sample_token_greedy(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates);
LLAMA_V3_API llama_v3_token llama_v3_sample_token(struct llama_v3_context * ctx, llama_v3_token_data_array * candidates);
LLAMA_V3_API void llama_v3_grammar_accept_token(struct llama_v3_context * ctx, struct llama_v3_grammar * grammar, llama_v3_token token);
LLAMA_V3_API struct llama_v3_timings llama_v3_get_timings(struct llama_v3_context * ctx);
LLAMA_V3_API void llama_v3_print_timings(struct llama_v3_context * ctx);
LLAMA_V3_API void llama_v3_reset_timings(struct llama_v3_context * ctx);
LLAMA_V3_API const char * llama_v3_print_system_info(void);
#ifdef __cplusplus
}
#endif
#ifdef LLAMA_V3_API_INTERNAL
#include <vector>
#include <string>
struct ggml_v3_tensor;
const std::vector<std::pair<std::string, struct ggml_v3_tensor *>>& llama_v3_internal_get_tensor_map(struct llama_v3_context * ctx);
#endif
#endif