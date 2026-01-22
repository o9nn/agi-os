#ifndef LLAMA_H
#define LLAMA_H
#ifdef LLAMA_SHARED
#    if defined(_WIN32) && !defined(__MINGW32__)
#        ifdef LLAMA_BUILD
#            define LLAMA_API __declspec(dllexport)
#        else
#            define LLAMA_API __declspec(dllimport)
#        endif
#    else
#        define LLAMA_API __attribute__ ((visibility ("default")))
#    endif
#else
#    define LLAMA_API
#endif
#ifdef __GNUC__
#    define DEPRECATED(func, hint) func __attribute__((deprecated(hint)))
#elif defined(_MSC_VER)
#    define DEPRECATED(func, hint) __declspec(deprecated(hint)) func
#else
#    define DEPRECATED(func, hint) func
#endif
#ifdef __cplusplus
extern "C" {
#endif
struct llama_model;
struct llama_context;
typedef int32_t llama_pos;
typedef int32_t llama_token;
typedef int32_t llama_seq_id;
enum llama_vocab_type {
LLAMA_VOCAB_TYPE_NONE = 0,
LLAMA_VOCAB_TYPE_SPM  = 1,
LLAMA_VOCAB_TYPE_BPE  = 2,
LLAMA_VOCAB_TYPE_WPM  = 3,
};
enum llama_vocab_pre_type {
LLAMA_VOCAB_PRE_TYPE_DEFAULT        = 0,
LLAMA_VOCAB_PRE_TYPE_LLAMA3         = 1,
LLAMA_VOCAB_PRE_TYPE_DEEPSEEK_LLM   = 2,
LLAMA_VOCAB_PRE_TYPE_DEEPSEEK_CODER = 3,
LLAMA_VOCAB_PRE_TYPE_FALCON         = 4,
LLAMA_VOCAB_PRE_TYPE_MPT            = 5,
LLAMA_VOCAB_PRE_TYPE_STARCODER      = 6,
LLAMA_VOCAB_PRE_TYPE_GPT2           = 7,
LLAMA_VOCAB_PRE_TYPE_REFACT         = 8,
LLAMA_VOCAB_PRE_TYPE_COMMAND_R      = 9,
LLAMA_VOCAB_PRE_TYPE_QWEN2          = 10,
LLAMA_VOCAB_PRE_TYPE_OLMO           = 11,
LLAMA_VOCAB_PRE_TYPE_DBRX           = 12,
};
enum llama_rope_type {
LLAMA_ROPE_TYPE_NONE = -1,
LLAMA_ROPE_TYPE_NORM =  0,
LLAMA_ROPE_TYPE_NEOX =  2,
LLAMA_ROPE_TYPE_GLM  =  4,
};
enum llama_token_type {
LLAMA_TOKEN_TYPE_UNDEFINED    = 0,
LLAMA_TOKEN_TYPE_NORMAL       = 1,
LLAMA_TOKEN_TYPE_UNKNOWN      = 2,
LLAMA_TOKEN_TYPE_CONTROL      = 3,
LLAMA_TOKEN_TYPE_USER_DEFINED = 4,
LLAMA_TOKEN_TYPE_UNUSED       = 5,
LLAMA_TOKEN_TYPE_BYTE         = 6,
};
enum llama_ftype {
LLAMA_FTYPE_ALL_F32              = 0,
LLAMA_FTYPE_MOSTLY_F16           = 1,
LLAMA_FTYPE_MOSTLY_Q4_0          = 2,
LLAMA_FTYPE_MOSTLY_Q4_1          = 3,
LLAMA_FTYPE_MOSTLY_Q4_1_SOME_F16 = 4,
LLAMA_FTYPE_MOSTLY_Q8_0          = 7,
LLAMA_FTYPE_MOSTLY_Q5_0          = 8,
LLAMA_FTYPE_MOSTLY_Q5_1          = 9,
LLAMA_FTYPE_MOSTLY_Q2_K          = 10,
LLAMA_FTYPE_MOSTLY_Q3_K_S        = 11,
LLAMA_FTYPE_MOSTLY_Q3_K_M        = 12,
LLAMA_FTYPE_MOSTLY_Q3_K_L        = 13,
LLAMA_FTYPE_MOSTLY_Q4_K_S        = 14,
LLAMA_FTYPE_MOSTLY_Q4_K_M        = 15,
LLAMA_FTYPE_MOSTLY_Q5_K_S        = 16,
LLAMA_FTYPE_MOSTLY_Q5_K_M        = 17,
LLAMA_FTYPE_MOSTLY_Q6_K          = 18,
LLAMA_FTYPE_MOSTLY_IQ2_XXS       = 19,
LLAMA_FTYPE_MOSTLY_IQ2_XS        = 20,
LLAMA_FTYPE_MOSTLY_Q2_K_S        = 21,
LLAMA_FTYPE_MOSTLY_IQ3_XS        = 22,
LLAMA_FTYPE_MOSTLY_IQ3_XXS       = 23,
LLAMA_FTYPE_MOSTLY_IQ1_S         = 24,
LLAMA_FTYPE_MOSTLY_IQ4_NL        = 25,
LLAMA_FTYPE_MOSTLY_IQ3_S         = 26,
LLAMA_FTYPE_MOSTLY_IQ3_M         = 27,
LLAMA_FTYPE_MOSTLY_IQ2_S         = 28,
LLAMA_FTYPE_MOSTLY_IQ2_M         = 29,
LLAMA_FTYPE_MOSTLY_IQ4_XS        = 30,
LLAMA_FTYPE_MOSTLY_IQ1_M         = 31,
LLAMA_FTYPE_MOSTLY_BF16          = 32,
LLAMA_FTYPE_GUESSED = 1024,
};
enum llama_rope_scaling_type {
LLAMA_ROPE_SCALING_TYPE_UNSPECIFIED = -1,
LLAMA_ROPE_SCALING_TYPE_NONE        = 0,
LLAMA_ROPE_SCALING_TYPE_LINEAR      = 1,
LLAMA_ROPE_SCALING_TYPE_YARN        = 2,
LLAMA_ROPE_SCALING_TYPE_MAX_VALUE   = LLAMA_ROPE_SCALING_TYPE_YARN,
};
enum llama_pooling_type {
LLAMA_POOLING_TYPE_UNSPECIFIED = -1,
LLAMA_POOLING_TYPE_NONE = 0,
LLAMA_POOLING_TYPE_MEAN = 1,
LLAMA_POOLING_TYPE_CLS  = 2,
};
enum llama_split_mode {
LLAMA_SPLIT_MODE_NONE    = 0,
LLAMA_SPLIT_MODE_LAYER   = 1,
LLAMA_SPLIT_MODE_ROW     = 2,
};
typedef struct llama_token_data {
llama_token id;
float logit;
float p;
} llama_token_data;
typedef struct llama_token_data_array {
llama_token_data * data;
size_t size;
bool sorted;
} llama_token_data_array;
typedef bool (*llama_progress_callback)(float progress, void * user_data);
typedef struct llama_batch {
int32_t n_tokens;
llama_token  *  token;
float        *  embd;
llama_pos    *  pos;
int32_t      *  n_seq_id;
llama_seq_id ** seq_id;
int8_t       *  logits;
llama_pos    all_pos_0;
llama_pos    all_pos_1;
llama_seq_id all_seq_id;
} llama_batch;
enum llama_model_kv_override_type {
LLAMA_KV_OVERRIDE_TYPE_INT,
LLAMA_KV_OVERRIDE_TYPE_FLOAT,
LLAMA_KV_OVERRIDE_TYPE_BOOL,
LLAMA_KV_OVERRIDE_TYPE_STR,
};
struct llama_model_kv_override {
enum llama_model_kv_override_type tag;
char key[128];
union {
int64_t val_i64;
double  val_f64;
bool    val_bool;
char    val_str[128];
};
};
struct llama_model_params {
int32_t n_gpu_layers;
enum llama_split_mode split_mode;
int32_t main_gpu;
const float * tensor_split;
llama_progress_callback progress_callback;
void * progress_callback_user_data;
const struct llama_model_kv_override * kv_overrides;
bool vocab_only;
bool use_mmap;
bool use_mlock;
bool check_tensors;
};
struct llama_context_params {
uint32_t seed;
uint32_t n_ctx;
uint32_t n_batch;
uint32_t n_ubatch;
uint32_t n_seq_max;
uint32_t n_threads;
uint32_t n_threads_batch;
enum llama_rope_scaling_type rope_scaling_type;
enum llama_pooling_type      pooling_type;
float    rope_freq_base;
float    rope_freq_scale;
float    yarn_ext_factor;
float    yarn_attn_factor;
float    yarn_beta_fast;
float    yarn_beta_slow;
uint32_t yarn_orig_ctx;
float    defrag_thold;
ggml_backend_sched_eval_callback cb_eval;
void * cb_eval_user_data;
enum ggml_type type_k;
enum ggml_type type_v;
bool logits_all;
bool embeddings;
bool offload_kqv;
bool flash_attn;
ggml_abort_callback abort_callback;
void *              abort_callback_data;
};
typedef struct llama_model_quantize_params {
int32_t nthread;
enum llama_ftype ftype;
enum ggml_type output_tensor_type;
enum ggml_type token_embedding_type;
bool allow_requantize;
bool quantize_output_tensor;
bool only_copy;
bool pure;
bool keep_split;
void * imatrix;
void * kv_overrides;
} llama_model_quantize_params;
struct llama_grammar;
enum llama_gretype {
LLAMA_GRETYPE_END            = 0,
LLAMA_GRETYPE_ALT            = 1,
LLAMA_GRETYPE_RULE_REF       = 2,
LLAMA_GRETYPE_CHAR           = 3,
LLAMA_GRETYPE_CHAR_NOT       = 4,
LLAMA_GRETYPE_CHAR_RNG_UPPER = 5,
LLAMA_GRETYPE_CHAR_ALT       = 6,
};
typedef struct llama_grammar_element {
enum llama_gretype type;
uint32_t           value;
} llama_grammar_element;
struct llama_timings {
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
typedef struct llama_chat_message {
const char * role;
const char * content;
} llama_chat_message;
LLAMA_API struct llama_model_params llama_model_default_params(void);
LLAMA_API struct llama_context_params llama_context_default_params(void);
LLAMA_API struct llama_model_quantize_params llama_model_quantize_default_params(void);
LLAMA_API void llama_backend_init(void);
LLAMA_API void llama_numa_init(enum ggml_numa_strategy numa);
LLAMA_API void llama_backend_free(void);
LLAMA_API struct llama_model * llama_load_model_from_file(
const char * path_model,
struct llama_model_params     params);
LLAMA_API void llama_free_model(struct llama_model * model);
LLAMA_API struct llama_context * llama_new_context_with_model(
struct llama_model * model,
struct llama_context_params   params);
LLAMA_API void llama_free(struct llama_context * ctx);
LLAMA_API int64_t llama_time_us(void);
LLAMA_API size_t llama_max_devices(void);
LLAMA_API bool llama_supports_mmap       (void);
LLAMA_API bool llama_supports_mlock      (void);
LLAMA_API bool llama_supports_gpu_offload(void);
LLAMA_API const struct llama_model * llama_get_model(const struct llama_context * ctx);
LLAMA_API uint32_t llama_n_ctx      (const struct llama_context * ctx);
LLAMA_API uint32_t llama_n_batch    (const struct llama_context * ctx);
LLAMA_API uint32_t llama_n_ubatch   (const struct llama_context * ctx);
LLAMA_API uint32_t llama_n_seq_max  (const struct llama_context * ctx);
LLAMA_API enum llama_pooling_type llama_pooling_type(const struct llama_context * ctx);
LLAMA_API enum llama_vocab_type   llama_vocab_type  (const struct llama_model   * model);
LLAMA_API enum llama_rope_type    llama_rope_type   (const struct llama_model   * model);
LLAMA_API int32_t llama_n_vocab    (const struct llama_model * model);
LLAMA_API int32_t llama_n_ctx_train(const struct llama_model * model);
LLAMA_API int32_t llama_n_embd     (const struct llama_model * model);
LLAMA_API int32_t llama_n_layer    (const struct llama_model * model);
LLAMA_API float llama_rope_freq_scale_train(const struct llama_model * model);
LLAMA_API int32_t llama_model_meta_val_str(const struct llama_model * model, const char * key, char * buf, size_t buf_size);
LLAMA_API int32_t llama_model_meta_count(const struct llama_model * model);
LLAMA_API int32_t llama_model_meta_key_by_index(const struct llama_model * model, int32_t i, char * buf, size_t buf_size);
LLAMA_API int32_t llama_model_meta_val_str_by_index(const struct llama_model * model, int32_t i, char * buf, size_t buf_size);
LLAMA_API int32_t llama_model_desc(const struct llama_model * model, char * buf, size_t buf_size);
LLAMA_API uint64_t llama_model_size(const struct llama_model * model);
LLAMA_API uint64_t llama_model_n_params(const struct llama_model * model);
LLAMA_API struct ggml_tensor * llama_get_model_tensor(struct llama_model * model, const char * name);
LLAMA_API uint32_t llama_model_quantize(
const char * fname_inp,
const char * fname_out,
const llama_model_quantize_params * params);
LLAMA_API int32_t llama_model_apply_lora_from_file(
const struct llama_model * model,
const char * path_lora,
float   scale,
const char * path_base_model,
int32_t   n_threads);
LLAMA_API int32_t llama_control_vector_apply(
struct llama_context * lctx,
const float * data,
size_t   len,
int32_t   n_embd,
int32_t   il_start,
int32_t   il_end);
struct llama_kv_cache_view_cell {
llama_pos pos;
};
struct llama_kv_cache_view {
int32_t n_cells;
int32_t n_seq_max;
int32_t token_count;
int32_t used_cells;
int32_t max_contiguous;
int32_t max_contiguous_idx;
struct llama_kv_cache_view_cell * cells;
llama_seq_id * cells_sequences;
};
LLAMA_API struct llama_kv_cache_view llama_kv_cache_view_init(const struct llama_context * ctx, int32_t n_seq_max);
LLAMA_API void llama_kv_cache_view_free(struct llama_kv_cache_view * view);
LLAMA_API void llama_kv_cache_view_update(const struct llama_context * ctx, struct llama_kv_cache_view * view);
LLAMA_API int32_t llama_get_kv_cache_token_count(const struct llama_context * ctx);
LLAMA_API int32_t llama_get_kv_cache_used_cells(const struct llama_context * ctx);
LLAMA_API void llama_kv_cache_clear(
struct llama_context * ctx);
LLAMA_API bool llama_kv_cache_seq_rm(
struct llama_context * ctx,
llama_seq_id   seq_id,
llama_pos   p0,
llama_pos   p1);
LLAMA_API void llama_kv_cache_seq_cp(
struct llama_context * ctx,
llama_seq_id   seq_id_src,
llama_seq_id   seq_id_dst,
llama_pos   p0,
llama_pos   p1);
LLAMA_API void llama_kv_cache_seq_keep(
struct llama_context * ctx,
llama_seq_id   seq_id);
LLAMA_API void llama_kv_cache_seq_add(
struct llama_context * ctx,
llama_seq_id   seq_id,
llama_pos   p0,
llama_pos   p1,
llama_pos   delta);
LLAMA_API void llama_kv_cache_seq_div(
struct llama_context * ctx,
llama_seq_id   seq_id,
llama_pos   p0,
llama_pos   p1,
int   d);
LLAMA_API llama_pos llama_kv_cache_seq_pos_max(
struct llama_context * ctx,
llama_seq_id   seq_id);
LLAMA_API void llama_kv_cache_defrag(struct llama_context * ctx);
LLAMA_API void llama_kv_cache_update(struct llama_context * ctx);
LLAMA_API size_t llama_state_get_size(const struct llama_context * ctx);
LLAMA_API DEPRECATED(size_t llama_get_state_size(const struct llama_context * ctx),
"use llama_state_get_size instead");
LLAMA_API size_t llama_state_get_data(
struct llama_context * ctx,
uint8_t * dst);
LLAMA_API DEPRECATED(size_t llama_copy_state_data(
struct llama_context * ctx,
uint8_t * dst),
"use llama_state_get_data instead");
LLAMA_API size_t llama_state_set_data(
struct llama_context * ctx,
const uint8_t * src);
LLAMA_API DEPRECATED(size_t llama_set_state_data(
struct llama_context * ctx,
const uint8_t * src),
"use llama_state_set_data instead");
LLAMA_API bool llama_state_load_file(
struct llama_context * ctx,
const char * path_session,
llama_token * tokens_out,
size_t   n_token_capacity,
size_t * n_token_count_out);
LLAMA_API DEPRECATED(bool llama_load_session_file(
struct llama_context * ctx,
const char * path_session,
llama_token * tokens_out,
size_t   n_token_capacity,
size_t * n_token_count_out),
"use llama_state_load_file instead");
LLAMA_API bool llama_state_save_file(
struct llama_context * ctx,
const char * path_session,
const llama_token * tokens,
size_t   n_token_count);
LLAMA_API DEPRECATED(bool llama_save_session_file(
struct llama_context * ctx,
const char * path_session,
const llama_token * tokens,
size_t   n_token_count),
"use llama_state_save_file instead");
LLAMA_API size_t llama_state_seq_get_size(
struct llama_context * ctx,
llama_seq_id   seq_id);
LLAMA_API size_t llama_state_seq_get_data(
struct llama_context * ctx,
uint8_t * dst,
llama_seq_id   seq_id);
LLAMA_API size_t llama_state_seq_set_data(
struct llama_context * ctx,
const uint8_t * src,
llama_seq_id   dest_seq_id);
LLAMA_API size_t llama_state_seq_save_file(
struct llama_context * ctx,
const char * filepath,
llama_seq_id   seq_id,
const llama_token * tokens,
size_t   n_token_count);
LLAMA_API size_t llama_state_seq_load_file(
struct llama_context * ctx,
const char * filepath,
llama_seq_id   dest_seq_id,
llama_token * tokens_out,
size_t   n_token_capacity,
size_t * n_token_count_out);
LLAMA_API struct llama_batch llama_batch_get_one(
llama_token * tokens,
int32_t   n_tokens,
llama_pos   pos_0,
llama_seq_id   seq_id);
LLAMA_API struct llama_batch llama_batch_init(
int32_t n_tokens,
int32_t embd,
int32_t n_seq_max);
LLAMA_API void llama_batch_free(struct llama_batch batch);
LLAMA_API int32_t llama_decode(
struct llama_context * ctx,
struct llama_batch   batch);
LLAMA_API void llama_set_n_threads(struct llama_context * ctx, uint32_t n_threads, uint32_t n_threads_batch);
LLAMA_API void llama_set_causal_attn(struct llama_context * ctx, bool causal_attn);
LLAMA_API void llama_set_abort_callback(struct llama_context * ctx, ggml_abort_callback abort_callback, void * abort_callback_data);
LLAMA_API void llama_synchronize(struct llama_context * ctx);
LLAMA_API float * llama_get_logits(struct llama_context * ctx);
LLAMA_API float * llama_get_logits_ith(struct llama_context * ctx, int32_t i);
LLAMA_API float * llama_get_embeddings(struct llama_context * ctx);
LLAMA_API float * llama_get_embeddings_ith(struct llama_context * ctx, int32_t i);
LLAMA_API float * llama_get_embeddings_seq(struct llama_context * ctx, llama_seq_id seq_id);
LLAMA_API const char * llama_token_get_text(const struct llama_model * model, llama_token token);
LLAMA_API float llama_token_get_score(const struct llama_model * model, llama_token token);
LLAMA_API enum llama_token_type llama_token_get_type(const struct llama_model * model, llama_token token);
LLAMA_API bool llama_token_is_eog(const struct llama_model * model, llama_token token);
LLAMA_API llama_token llama_token_bos(const struct llama_model * model);
LLAMA_API llama_token llama_token_eos(const struct llama_model * model);
LLAMA_API llama_token llama_token_cls(const struct llama_model * model);
LLAMA_API llama_token llama_token_sep(const struct llama_model * model);
LLAMA_API llama_token llama_token_nl (const struct llama_model * model);
LLAMA_API int32_t         llama_add_bos_token(const struct llama_model * model);
LLAMA_API int32_t         llama_add_eos_token(const struct llama_model * model);
LLAMA_API llama_token llama_token_prefix(const struct llama_model * model);
LLAMA_API llama_token llama_token_middle(const struct llama_model * model);
LLAMA_API llama_token llama_token_suffix(const struct llama_model * model);
LLAMA_API llama_token llama_token_eot   (const struct llama_model * model);
LLAMA_API int32_t llama_tokenize(
const struct llama_model * model,
const char * text,
int32_t   text_len,
llama_token * tokens,
int32_t   n_tokens_max,
bool   add_special,
bool   parse_special);
LLAMA_API int32_t llama_token_to_piece(
const struct llama_model * model,
llama_token   token,
char * buf,
int32_t   length,
bool   special);
LLAMA_API int32_t llama_chat_apply_template(
const struct llama_model * model,
const char * tmpl,
const struct llama_chat_message * chat,
size_t   n_msg,
bool   add_ass,
char * buf,
int32_t   length);
LLAMA_API struct llama_grammar * llama_grammar_init(
const llama_grammar_element ** rules,
size_t    n_rules,
size_t    start_rule_index);
LLAMA_API void llama_grammar_free(struct llama_grammar * grammar);
LLAMA_API struct llama_grammar * llama_grammar_copy(const struct llama_grammar * grammar);
LLAMA_API void llama_set_rng_seed(struct llama_context * ctx, uint32_t seed);
LLAMA_API void llama_sample_repetition_penalties(
struct llama_context * ctx,
llama_token_data_array * candidates,
const llama_token * last_tokens,
size_t   penalty_last_n,
float   penalty_repeat,
float   penalty_freq,
float   penalty_present);
LLAMA_API void llama_sample_apply_guidance(
struct llama_context * ctx,
float * logits,
float * logits_guidance,
float   scale);
LLAMA_API void llama_sample_softmax(
struct llama_context * ctx,
llama_token_data_array * candidates);
LLAMA_API void llama_sample_top_k(
struct llama_context * ctx,
llama_token_data_array * candidates,
int32_t   k,
size_t   min_keep);
LLAMA_API void llama_sample_top_p(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   p,
size_t   min_keep);
LLAMA_API void llama_sample_min_p(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   p,
size_t   min_keep);
LLAMA_API void llama_sample_tail_free(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   z,
size_t   min_keep);
LLAMA_API void llama_sample_typical(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   p,
size_t   min_keep);
LLAMA_API void llama_sample_entropy(
struct llama_context * ctx,
llama_token_data_array * candidates_p,
float   min_temp,
float   max_temp,
float   exponent_val);
LLAMA_API void llama_sample_temp(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   temp);
LLAMA_API void llama_sample_grammar(
struct llama_context * ctx,
llama_token_data_array * candidates,
const struct llama_grammar * grammar);
LLAMA_API llama_token llama_sample_token_mirostat(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   tau,
float   eta,
int32_t   m,
float * mu);
LLAMA_API llama_token llama_sample_token_mirostat_v2(
struct llama_context * ctx,
llama_token_data_array * candidates,
float   tau,
float   eta,
float * mu);
LLAMA_API llama_token llama_sample_token_greedy(
struct llama_context * ctx,
llama_token_data_array * candidates);
LLAMA_API llama_token llama_sample_token(
struct llama_context * ctx,
llama_token_data_array * candidates);
LLAMA_API void llama_grammar_accept_token(
struct llama_context * ctx,
struct llama_grammar * grammar,
llama_token   token);
struct llama_beam_view {
const llama_token * tokens;
size_t n_tokens;
float  p;
bool   eob;
};
struct llama_beams_state {
struct llama_beam_view * beam_views;
size_t n_beams;
size_t common_prefix_length;
bool   last_call;
};
typedef void (*llama_beam_search_callback_fn_t)(void * callback_data, struct llama_beams_state);
LLAMA_API void llama_beam_search(
struct llama_context * ctx,
llama_beam_search_callback_fn_t   callback,
void * callback_data,
size_t   n_beams,
int32_t   n_past,
int32_t   n_predict);
LLAMA_API int llama_split_path(char * split_path, size_t maxlen, const char * path_prefix, int split_no, int split_count);
LLAMA_API int llama_split_prefix(char * split_prefix, size_t maxlen, const char * split_path, int split_no, int split_count);
LLAMA_API struct llama_timings llama_get_timings(struct llama_context * ctx);
LLAMA_API void llama_print_timings(struct llama_context * ctx);
LLAMA_API void llama_reset_timings(struct llama_context * ctx);
LLAMA_API const char * llama_print_system_info(void);
LLAMA_API void llama_log_set(ggml_log_callback log_callback, void * user_data);
LLAMA_API void llama_dump_timing_info_yaml(FILE * stream, const struct llama_context * ctx);
#ifdef __cplusplus
}
#endif
#endif