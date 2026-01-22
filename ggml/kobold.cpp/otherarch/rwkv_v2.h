#ifndef RWKV_H2
#define RWKV_H2
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#ifdef RWKV_SHARED2
# if defined(_WIN32) && !defined(__MINGW32__)
# ifdef RWKV_BUILD
# define RWKV_V2_API __declspec(dllexport)
# else
# define RWKV_V2_API __declspec(dllimport)
# endif
# else
# define RWKV_V2_API __attribute__ ((visibility ("default")))
# endif
#else
# define RWKV_V2_API
#endif
#define RWKV_V2_FILE_MAGIC 0x67676d66
#define RWKV_V2_FILE_VERSION 100
#ifdef __cplusplus
extern "C" {
#endif
struct rwkv_v2_context;
RWKV_V2_API struct rwkv_v2_context * rwkv_v2_init_from_file(const char * model_file_path, uint32_t n_threads);
RWKV_V2_API bool rwkv_v2_eval(struct rwkv_v2_context * ctx, int32_t token, float * state_in, float * state_out, float * logits_out);
RWKV_V2_API uint32_t rwkv_v2_get_state_buffer_element_count(struct rwkv_v2_context * ctx);
RWKV_V2_API uint32_t rwkv_v2_get_logits_buffer_element_count(struct rwkv_v2_context * ctx);
RWKV_V2_API void rwkv_v2_free(struct rwkv_v2_context * ctx);
RWKV_V2_API bool rwkv_v2_quantize_model_file(const char * model_file_path_in, const char * model_file_path_out, const char * format_name);
RWKV_V2_API const char * rwkv_v2_get_system_info_string(void);
#ifdef __cplusplus
}
#endif
#endif