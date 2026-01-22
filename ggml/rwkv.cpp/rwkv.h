#ifndef RWKV_H
#define RWKV_H
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#if defined(RWKV_SHARED)
#    if defined(_WIN32) && !defined(__MINGW32__)
#        if defined(RWKV_BUILD)
#            define RWKV_API __declspec(dllexport)
#        else
#            define RWKV_API __declspec(dllimport)
#        endif
#    else
#        define RWKV_API __attribute__ ((visibility ("default")))
#    endif
#else
#    define RWKV_API
#endif
#define RWKV_FILE_MAGIC 0x67676d66
#define RWKV_FILE_VERSION_0 100
#define RWKV_FILE_VERSION_1 101
#define RWKV_FILE_VERSION_MIN RWKV_FILE_VERSION_0
#define RWKV_FILE_VERSION_MAX RWKV_FILE_VERSION_1
#define RWKV_FILE_VERSION RWKV_FILE_VERSION_MAX
#if defined(__cplusplus)
extern "C" {
#endif
enum rwkv_error_flags {
RWKV_ERROR_NONE = 0,
RWKV_ERROR_ARGS = 1 << 8,
RWKV_ERROR_FILE = 2 << 8,
RWKV_ERROR_MODEL = 3 << 8,
RWKV_ERROR_MODEL_PARAMS = 4 << 8,
RWKV_ERROR_GRAPH = 5 << 8,
RWKV_ERROR_CTX = 6 << 8,
RWKV_ERROR_ALLOC = 1,
RWKV_ERROR_FILE_OPEN = 2,
RWKV_ERROR_FILE_STAT = 3,
RWKV_ERROR_FILE_READ = 4,
RWKV_ERROR_FILE_WRITE = 5,
RWKV_ERROR_FILE_MAGIC = 6,
RWKV_ERROR_FILE_VERSION = 7,
RWKV_ERROR_DATA_TYPE = 8,
RWKV_ERROR_UNSUPPORTED = 9,
RWKV_ERROR_SHAPE = 10,
RWKV_ERROR_DIMENSION = 11,
RWKV_ERROR_KEY = 12,
RWKV_ERROR_DATA = 13,
RWKV_ERROR_PARAM_MISSING = 14
};
struct rwkv_context;
RWKV_API void rwkv_set_print_errors(struct rwkv_context * ctx, const bool print_errors);
RWKV_API bool rwkv_get_print_errors(const struct rwkv_context * ctx);
RWKV_API enum rwkv_error_flags rwkv_get_last_error(struct rwkv_context * ctx);
RWKV_API struct rwkv_context * rwkv_init_from_file(const char * model_file_path, const uint32_t n_threads, const uint32_t n_gpu_layers);
RWKV_API struct rwkv_context * rwkv_clone_context(struct rwkv_context * ctx, const uint32_t n_threads);
RWKV_API bool rwkv_eval(
struct rwkv_context * ctx,
const uint32_t token,
const float * state_in,
float * state_out,
float * logits_out
);
RWKV_API bool rwkv_eval_sequence(
struct rwkv_context * ctx,
const uint32_t * tokens,
const size_t sequence_len,
const float * state_in,
float * state_out,
float * logits_out
);
RWKV_API bool rwkv_eval_sequence_in_chunks(
struct rwkv_context * ctx,
const uint32_t * tokens,
const size_t sequence_len,
const size_t chunk_size,
const float * state_in,
float * state_out,
float * logits_out
);
RWKV_API size_t rwkv_get_n_vocab(const struct rwkv_context * ctx);
RWKV_API size_t rwkv_get_n_embed(const struct rwkv_context * ctx);
RWKV_API size_t rwkv_get_n_layer(const struct rwkv_context * ctx);
RWKV_API size_t rwkv_get_state_len(const struct rwkv_context * ctx);
RWKV_API size_t rwkv_get_logits_len(const struct rwkv_context * ctx);
RWKV_API void rwkv_init_state(const struct rwkv_context * ctx, float * state);
RWKV_API void rwkv_free(struct rwkv_context * ctx);
RWKV_API bool rwkv_quantize_model_file(const char * model_file_path_in, const char * model_file_path_out, const char * format_name);
RWKV_API const char * rwkv_get_system_info_string(void);
#if defined(__cplusplus)
}
#endif
#endif