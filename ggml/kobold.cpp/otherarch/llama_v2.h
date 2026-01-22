#ifndef LLAMA_V2_H
#define LLAMA_V2_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef LLAMA_V2_SHARED
# if defined(_WIN32) && !defined(__MINGW32__)
# ifdef LLAMA_V2_BUILD
# define LLAMA_V2_API __declspec(dllexport)
# else
# define LLAMA_V2_API __declspec(dllimport)
# endif
# else
# define LLAMA_V2_API __attribute__ ((visibility ("default")))
# endif
#else
# define LLAMA_V2_API
#endif
#define LLAMA_V2_FILE_VERSION 3
#define LLAMA_V2_FILE_MAGIC 'ggjt'
#define LLAMA_V2_FILE_MAGIC_UNVERSIONED 'ggml'
#define LLAMA_V2_SESSION_MAGIC 'ggsn'
#define LLAMA_V2_SESSION_VERSION 1
#ifdef __cplusplus
extern "C" {
#endif
struct llama_v2_context;
typedef int llama_v2_token;
typedef struct llama_v2_token_data {
llama_v2_token id;
float logit;
float p;
} llama_v2_token_data;
typedef struct llama_v2_token_data_array {
llama_v2_token_data * data;
size_t size;
bool sorted;
} llama_v2_token_data_array;
typedef void (*llama_v2_progress_callback)(float progress, void *ctx);
struct llama_v2_context_params {
int n_ctx;
int n_gpu_layers;
int seed;
bool f16_kv;
bool logits_all;
bool vocab_only;
bool use_mmap;
bool use_mlock;
bool embedding;
llama_v2_progress_callback progress_callback;
void * progress_callback_user_data;
};
enum llama_v2_ftype {
LLAMA_V2_FTYPE_ALL_F32 = 0,
LLAMA_V2_FTYPE_MOSTLY_F16 = 1,
LLAMA_V2_FTYPE_MOSTLY_Q4_0 = 2,
LLAMA_V2_FTYPE_MOSTLY_Q4_1 = 3,
LLAMA_V2_FTYPE_MOSTLY_Q4_1_SOME_F16 = 4,
LLAMA_V2_FTYPE_MOSTLY_Q4_2 = 5,
LLAMA_V2_FTYPE_MOSTLY_Q4_3 = 6,
LLAMA_V2_FTYPE_MOSTLY_Q8_0 = 7,
LLAMA_V2_FTYPE_MOSTLY_Q5_0 = 8,
LLAMA_V2_FTYPE_MOSTLY_Q5_1 = 9,
};
LLAMA_V2_API struct llama_v2_context_params llama_v2_context_default_params();
LLAMA_V2_API bool llama_v2_mmap_supported();
LLAMA_V2_API bool llama_v2_mlock_supported();
LLAMA_V2_API struct llama_v2_context * llama_v2_init_from_file(
const char * path_model,
struct llama_v2_context_params params);
LLAMA_V2_API void llama_v2_free(struct llama_v2_context * ctx);
LLAMA_V2_API int llama_v2_model_quantize(
const char * fname_inp,
const char * fname_out,
enum llama_v2_ftype ftype,
int nthread);
LLAMA_V2_API int llama_v2_apply_lora_from_file(
struct llama_v2_context * ctx,
const char * path_lora,
const char * path_base_model,
int n_threads);
LLAMA_V2_API int llama_v2_get_kv_cache_token_count(const struct llama_v2_context * ctx);
LLAMA_V2_API void llama_v2_set_rng_seed(struct llama_v2_context * ctx, int seed);
LLAMA_V2_API size_t llama_v2_get_state_size(const struct llama_v2_context * ctx);
LLAMA_V2_API size_t llama_v2_copy_state_data(struct llama_v2_context * ctx, uint8_t * dst);
LLAMA_V2_API size_t llama_v2_set_state_data(struct llama_v2_context * ctx, const uint8_t * src);
LLAMA_V2_API int llama_v2_eval(
struct llama_v2_context * ctx,
const llama_v2_token * tokens,
int n_tokens,
int n_past,
int n_threads);
LLAMA_V2_API int llama_v2_tokenize(
struct llama_v2_context * ctx,
const char * text,
llama_v2_token * tokens,
int n_max_tokens,
bool add_bos);
std::vector<llama_v2_token> legacy_llama_v2_tokenize(struct llama_v2_context * ctx, const std::string & text, bool add_bos);
LLAMA_V2_API int llama_v2_n_vocab(const struct llama_v2_context * ctx);
LLAMA_V2_API int llama_v2_n_ctx (const struct llama_v2_context * ctx);
LLAMA_V2_API int llama_v2_n_embd (const struct llama_v2_context * ctx);
LLAMA_V2_API float * llama_v2_get_logits(struct llama_v2_context * ctx);
LLAMA_V2_API float * llama_v2_get_embeddings(struct llama_v2_context * ctx);
LLAMA_V2_API const char * llama_v2_token_to_str(const struct llama_v2_context * ctx, llama_v2_token token);
LLAMA_V2_API llama_v2_token llama_v2_token_bos();
LLAMA_V2_API llama_v2_token llama_v2_token_eos();
LLAMA_V2_API llama_v2_token llama_v2_token_nl();
LLAMA_V2_API void llama_v2_sample_repetition_penalty(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, const llama_v2_token * last_tokens, size_t last_tokens_size, float penalty);
LLAMA_V2_API void llama_v2_sample_frequency_and_presence_penalties(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, const llama_v2_token * last_tokens, size_t last_tokens_size, float alpha_frequency, float alpha_presence);
LLAMA_V2_API void llama_v2_sample_softmax(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates);
LLAMA_V2_API void llama_v2_sample_top_k(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, int k, size_t min_keep);
LLAMA_V2_API void llama_v2_sample_top_p(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float p, size_t min_keep);
LLAMA_V2_API void llama_v2_sample_tail_free(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float z, size_t min_keep);
LLAMA_V2_API void llama_v2_sample_typical(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float p, size_t min_keep);
LLAMA_V2_API void llama_v2_sample_temperature(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float temp);
LLAMA_V2_API llama_v2_token llama_v2_sample_token_mirostat(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float tau, float eta, int m, float * mu);
LLAMA_V2_API llama_v2_token llama_v2_sample_token_mirostat_v2(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates, float tau, float eta, float * mu);
LLAMA_V2_API llama_v2_token llama_v2_sample_token_greedy(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates);
LLAMA_V2_API llama_v2_token llama_v2_sample_token(struct llama_v2_context * ctx, llama_v2_token_data_array * candidates);
LLAMA_V2_API void llama_v2_print_timings(struct llama_v2_context * ctx);
LLAMA_V2_API void llama_v2_reset_timings(struct llama_v2_context * ctx);
LLAMA_V2_API const char * llama_v2_print_system_info(void);
#ifdef __cplusplus
}
#endif
#ifdef LLAMA_V2_API_INTERNAL
#include <vector>
#include <string>
struct ggml_v2_tensor;
std::vector<std::pair<std::string, struct ggml_v2_tensor *>>& llama_v2_internal_get_tensor_map(struct llama_v2_context * ctx);
#endif
#endif