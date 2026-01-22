#ifndef WHISPER_H
#define WHISPER_H
#include "ggml.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string>
#ifdef __GNUC__
#    define WHISPER_DEPRECATED(func, hint) func __attribute__((deprecated(hint)))
#elif defined(_MSC_VER)
#    define WHISPER_DEPRECATED(func, hint) __declspec(deprecated(hint)) func
#else
#    define WHISPER_DEPRECATED(func, hint) func
#endif
#ifdef WHISPER_SHARED
#    ifdef _WIN32
#        ifdef WHISPER_BUILD
#            define WHISPER_API __declspec(dllexport)
#        else
#            define WHISPER_API __declspec(dllimport)
#        endif
#    else
#        define WHISPER_API __attribute__ ((visibility ("default")))
#    endif
#else
#    define WHISPER_API
#endif
#define WHISPER_SAMPLE_RATE 16000
#define WHISPER_N_FFT       400
#define WHISPER_HOP_LENGTH  160
#define WHISPER_CHUNK_SIZE  30
#ifdef __cplusplus
extern "C" {
#endif
struct whisper_context;
struct whisper_state;
struct whisper_full_params;
typedef int32_t whisper_pos;
typedef int32_t whisper_token;
typedef int32_t whisper_seq_id;
enum whisper_alignment_heads_preset {
WHISPER_AHEADS_NONE,
WHISPER_AHEADS_N_TOP_MOST,
WHISPER_AHEADS_CUSTOM,
WHISPER_AHEADS_TINY_EN,
WHISPER_AHEADS_TINY,
WHISPER_AHEADS_BASE_EN,
WHISPER_AHEADS_BASE,
WHISPER_AHEADS_SMALL_EN,
WHISPER_AHEADS_SMALL,
WHISPER_AHEADS_MEDIUM_EN,
WHISPER_AHEADS_MEDIUM,
WHISPER_AHEADS_LARGE_V1,
WHISPER_AHEADS_LARGE_V2,
WHISPER_AHEADS_LARGE_V3,
};
typedef struct whisper_ahead {
int n_text_layer;
int n_head;
} whisper_ahead;
typedef struct whisper_aheads {
size_t n_heads;
const whisper_ahead * heads;
} whisper_aheads;
struct whisper_context_params {
bool  use_gpu;
bool  flash_attn;
int   gpu_device;
bool dtw_token_timestamps;
enum whisper_alignment_heads_preset dtw_aheads_preset;
int dtw_n_top;
struct whisper_aheads dtw_aheads;
size_t dtw_mem_size;
};
typedef struct whisper_token_data {
whisper_token id;
whisper_token tid;
float p;
float plog;
float pt;
float ptsum;
int64_t t0;
int64_t t1;
int64_t t_dtw;
float vlen;
} whisper_token_data;
typedef struct whisper_model_loader {
void * context;
size_t (*read)(void * ctx, void * output, size_t read_size);
bool    (*eof)(void * ctx);
void  (*close)(void * ctx);
} whisper_model_loader;
enum whisper_gretype {
WHISPER_GRETYPE_END            = 0,
WHISPER_GRETYPE_ALT            = 1,
WHISPER_GRETYPE_RULE_REF       = 2,
WHISPER_GRETYPE_CHAR           = 3,
WHISPER_GRETYPE_CHAR_NOT       = 4,
WHISPER_GRETYPE_CHAR_RNG_UPPER = 5,
WHISPER_GRETYPE_CHAR_ALT       = 6,
};
typedef struct whisper_grammar_element {
enum whisper_gretype type;
uint32_t             value;
} whisper_grammar_element;
WHISPER_API struct whisper_context * whisper_init_from_file_with_params  (const char * path_model,              struct whisper_context_params params);
WHISPER_API struct whisper_context * whisper_init_from_buffer_with_params(void * buffer, size_t buffer_size,    struct whisper_context_params params);
WHISPER_API struct whisper_context * whisper_init_with_params            (struct whisper_model_loader * loader, struct whisper_context_params params);
WHISPER_API struct whisper_context * whisper_init_from_file_with_params_no_state  (const char * path_model,              struct whisper_context_params params);
WHISPER_API struct whisper_context * whisper_init_from_buffer_with_params_no_state(void * buffer, size_t buffer_size,    struct whisper_context_params params);
WHISPER_API struct whisper_context * whisper_init_with_params_no_state            (struct whisper_model_loader * loader, struct whisper_context_params params);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init_from_file(const char * path_model),
"use whisper_init_from_file_with_params instead"
);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init_from_buffer(void * buffer, size_t buffer_size),
"use whisper_init_from_buffer_with_params instead"
);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init(struct whisper_model_loader * loader),
"use whisper_init_with_params instead"
);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init_from_file_no_state(const char * path_model),
"use whisper_init_from_file_with_params_no_state instead"
);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init_from_buffer_no_state(void * buffer, size_t buffer_size),
"use whisper_init_from_buffer_with_params_no_state instead"
);
WHISPER_DEPRECATED(
WHISPER_API struct whisper_context * whisper_init_no_state(struct whisper_model_loader * loader),
"use whisper_init_with_params_no_state instead"
);
WHISPER_API struct whisper_state * whisper_init_state(struct whisper_context * ctx);
WHISPER_API int whisper_ctx_init_openvino_encoder(
struct whisper_context * ctx,
const char * model_path,
const char * device,
const char * cache_dir);
WHISPER_API void whisper_free      (struct whisper_context * ctx);
WHISPER_API void whisper_free_state(struct whisper_state * state);
WHISPER_API void whisper_free_params(struct whisper_full_params * params);
WHISPER_API void whisper_free_context_params(struct whisper_context_params * params);
WHISPER_API int whisper_pcm_to_mel(
struct whisper_context * ctx,
const float * samples,
int   n_samples,
int   n_threads);
WHISPER_API int whisper_pcm_to_mel_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
const float * samples,
int   n_samples,
int   n_threads);
WHISPER_API int whisper_pcm_to_mel_phase_vocoder(
struct whisper_context * ctx,
const float * samples,
int   n_samples,
int   n_threads);
WHISPER_API int whisper_pcm_to_mel_phase_vocoder_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
const float * samples,
int   n_samples,
int   n_threads);
WHISPER_API int whisper_set_mel(
struct whisper_context * ctx,
const float * data,
int   n_len,
int   n_mel);
WHISPER_API int whisper_set_mel_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
const float * data,
int   n_len,
int   n_mel);
WHISPER_API int whisper_encode(
struct whisper_context * ctx,
int   offset,
int   n_threads);
WHISPER_API int whisper_encode_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
int   offset,
int   n_threads);
WHISPER_API int whisper_decode(
struct whisper_context * ctx,
const whisper_token * tokens,
int   n_tokens,
int   n_past,
int   n_threads);
WHISPER_API int whisper_decode_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
const whisper_token * tokens,
int   n_tokens,
int   n_past,
int   n_threads);
WHISPER_API int whisper_tokenize(
struct whisper_context * ctx,
const char * text,
whisper_token * tokens,
int   n_max_tokens);
int whisper_token_count(struct whisper_context * ctx, const char * text);
WHISPER_API int whisper_lang_max_id();
WHISPER_API int whisper_lang_id(const char * lang);
WHISPER_API const char * whisper_lang_str(int id);
WHISPER_API const char * whisper_lang_str_full(int id);
WHISPER_API int whisper_lang_auto_detect(
struct whisper_context * ctx,
int   offset_ms,
int   n_threads,
float * lang_probs);
WHISPER_API int whisper_lang_auto_detect_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
int   offset_ms,
int   n_threads,
float * lang_probs);
WHISPER_API int whisper_n_len           (struct whisper_context * ctx);
WHISPER_API int whisper_n_len_from_state(struct whisper_state * state);
WHISPER_API int whisper_n_vocab         (struct whisper_context * ctx);
WHISPER_API int whisper_n_text_ctx      (struct whisper_context * ctx);
WHISPER_API int whisper_n_audio_ctx     (struct whisper_context * ctx);
WHISPER_API int whisper_is_multilingual (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_vocab      (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_audio_ctx  (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_audio_state(struct whisper_context * ctx);
WHISPER_API int whisper_model_n_audio_head (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_audio_layer(struct whisper_context * ctx);
WHISPER_API int whisper_model_n_text_ctx   (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_text_state (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_text_head  (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_text_layer (struct whisper_context * ctx);
WHISPER_API int whisper_model_n_mels       (struct whisper_context * ctx);
WHISPER_API int whisper_model_ftype        (struct whisper_context * ctx);
WHISPER_API int whisper_model_type         (struct whisper_context * ctx);
WHISPER_API float * whisper_get_logits           (struct whisper_context * ctx);
WHISPER_API float * whisper_get_logits_from_state(struct whisper_state * state);
WHISPER_API const char * whisper_token_to_str(struct whisper_context * ctx, whisper_token token);
WHISPER_API const char * whisper_model_type_readable(struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_eot (struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_sot (struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_solm(struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_prev(struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_nosp(struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_not (struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_beg (struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_lang(struct whisper_context * ctx, int lang_id);
WHISPER_API whisper_token whisper_token_translate (struct whisper_context * ctx);
WHISPER_API whisper_token whisper_token_transcribe(struct whisper_context * ctx);
WHISPER_API void whisper_print_timings(struct whisper_context * ctx);
WHISPER_API void whisper_reset_timings(struct whisper_context * ctx);
WHISPER_API const char * whisper_print_system_info(void);
enum whisper_sampling_strategy {
WHISPER_SAMPLING_GREEDY,
WHISPER_SAMPLING_BEAM_SEARCH,
};
typedef void (*whisper_new_segment_callback)(struct whisper_context * ctx, struct whisper_state * state, int n_new, void * user_data);
typedef void (*whisper_progress_callback)(struct whisper_context * ctx, struct whisper_state * state, int progress, void * user_data);
typedef bool (*whisper_encoder_begin_callback)(struct whisper_context * ctx, struct whisper_state * state, void * user_data);
typedef void (*whisper_logits_filter_callback)(
struct whisper_context * ctx,
struct whisper_state * state,
const whisper_token_data * tokens,
int   n_tokens,
float * logits,
void * user_data);
struct whisper_full_params {
enum whisper_sampling_strategy strategy;
int n_threads;
int n_max_text_ctx;
int offset_ms;
int duration_ms;
bool translate;
bool no_context;
bool no_timestamps;
bool single_segment;
bool print_special;
bool print_progress;
bool print_realtime;
bool print_timestamps;
bool  token_timestamps;
float thold_pt;
float thold_ptsum;
int   max_len;
bool  split_on_word;
int   max_tokens;
bool speed_up;
bool debug_mode;
int  audio_ctx;
bool tdrz_enable;
const char * suppress_regex;
const char * initial_prompt;
const whisper_token * prompt_tokens;
int prompt_n_tokens;
const char * language;
bool detect_language;
bool suppress_blank;
bool suppress_non_speech_tokens;
float temperature;
float max_initial_ts;
float length_penalty;
float temperature_inc;
float entropy_thold;
float logprob_thold;
float no_speech_thold;
struct {
int best_of;
} greedy;
struct {
int beam_size;
float patience;
} beam_search;
whisper_new_segment_callback new_segment_callback;
void * new_segment_callback_user_data;
whisper_progress_callback progress_callback;
void * progress_callback_user_data;
whisper_encoder_begin_callback encoder_begin_callback;
void * encoder_begin_callback_user_data;
ggml_abort_callback abort_callback;
void * abort_callback_user_data;
whisper_logits_filter_callback logits_filter_callback;
void * logits_filter_callback_user_data;
const whisper_grammar_element ** grammar_rules;
size_t                           n_grammar_rules;
size_t                           i_start_rule;
float                            grammar_penalty;
};
WHISPER_API struct whisper_context_params * whisper_context_default_params_by_ref();
WHISPER_API struct whisper_context_params whisper_context_default_params(void);
WHISPER_API struct whisper_full_params * whisper_full_default_params_by_ref(enum whisper_sampling_strategy strategy);
WHISPER_API struct whisper_full_params whisper_full_default_params(enum whisper_sampling_strategy strategy);
WHISPER_API int whisper_full(
struct whisper_context * ctx,
struct whisper_full_params   params,
const float * samples,
int   n_samples);
WHISPER_API int whisper_full_with_state(
struct whisper_context * ctx,
struct whisper_state * state,
struct whisper_full_params   params,
const float * samples,
int   n_samples);
WHISPER_API int whisper_full_parallel(
struct whisper_context * ctx,
struct whisper_full_params   params,
const float * samples,
int   n_samples,
int   n_processors);
WHISPER_API int whisper_full_n_segments           (struct whisper_context * ctx);
WHISPER_API int whisper_full_n_segments_from_state(struct whisper_state * state);
WHISPER_API int whisper_full_lang_id(struct whisper_context * ctx);
WHISPER_API int whisper_full_lang_id_from_state(struct whisper_state * state);
WHISPER_API int64_t whisper_full_get_segment_t0           (struct whisper_context * ctx, int i_segment);
WHISPER_API int64_t whisper_full_get_segment_t0_from_state(struct whisper_state * state, int i_segment);
WHISPER_API int64_t whisper_full_get_segment_t1           (struct whisper_context * ctx, int i_segment);
WHISPER_API int64_t whisper_full_get_segment_t1_from_state(struct whisper_state * state, int i_segment);
WHISPER_API bool whisper_full_get_segment_speaker_turn_next(struct whisper_context * ctx, int i_segment);
WHISPER_API bool whisper_full_get_segment_speaker_turn_next_from_state(struct whisper_state * state, int i_segment);
WHISPER_API const char * whisper_full_get_segment_text           (struct whisper_context * ctx, int i_segment);
WHISPER_API const char * whisper_full_get_segment_text_from_state(struct whisper_state * state, int i_segment);
WHISPER_API int whisper_full_n_tokens           (struct whisper_context * ctx, int i_segment);
WHISPER_API int whisper_full_n_tokens_from_state(struct whisper_state * state, int i_segment);
WHISPER_API const char * whisper_full_get_token_text           (struct whisper_context * ctx, int i_segment, int i_token);
WHISPER_API const char * whisper_full_get_token_text_from_state(struct whisper_context * ctx, struct whisper_state * state, int i_segment, int i_token);
WHISPER_API whisper_token whisper_full_get_token_id           (struct whisper_context * ctx, int i_segment, int i_token);
WHISPER_API whisper_token whisper_full_get_token_id_from_state(struct whisper_state * state, int i_segment, int i_token);
WHISPER_API whisper_token_data whisper_full_get_token_data           (struct whisper_context * ctx, int i_segment, int i_token);
WHISPER_API whisper_token_data whisper_full_get_token_data_from_state(struct whisper_state * state, int i_segment, int i_token);
WHISPER_API float whisper_full_get_token_p           (struct whisper_context * ctx, int i_segment, int i_token);
WHISPER_API float whisper_full_get_token_p_from_state(struct whisper_state * state, int i_segment, int i_token);
WHISPER_API int          whisper_bench_memcpy          (int n_threads);
WHISPER_API const char * whisper_bench_memcpy_str      (int n_threads);
WHISPER_API int          whisper_bench_ggml_mul_mat    (int n_threads);
WHISPER_API const char * whisper_bench_ggml_mul_mat_str(int n_threads);
WHISPER_API void whisper_log_set(ggml_log_callback log_callback, void * user_data);
#ifdef __cplusplus
}
#endif
#endif
std::string to_timestamp(int64_t t, bool comma = false);
int timestamp_to_sample(int64_t t, int n_samples, int whisper_sample_rate);
bool is_file_exist(const char *fileName);