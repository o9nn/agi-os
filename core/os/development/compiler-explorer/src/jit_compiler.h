#ifndef HURDCOG_JIT_COMPILER_H
#define HURDCOG_JIT_COMPILER_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdint.h>
#include <stdbool.h>
typedef struct {
int optimization_level;
bool enable_cognitive;
bool enable_logging;
const char* target_arch;
const char* log_file;
} jit_config_t;
typedef struct jit_context jit_context_t;
typedef enum {
JIT_SUCCESS = 0,
JIT_ERROR_INIT = -1,
JIT_ERROR_COMPILE = -2,
JIT_ERROR_EXECUTE = -3,
JIT_ERROR_INVALID_CODE = -4,
JIT_ERROR_OUT_OF_MEMORY = -5,
JIT_ERROR_UNSUPPORTED = -6
} jit_error_t;
typedef struct {
uint64_t compile_time_us;
uint64_t code_size_bytes;
uint32_t optimization_passes;
uint32_t cognitive_hints;
} jit_stats_t;
jit_context_t* jit_init(const jit_config_t* config);
jit_error_t jit_compile(jit_context_t* ctx,
const char* source_code,
const char* entry_function,
jit_stats_t* stats);
jit_error_t jit_execute(jit_context_t* ctx,
const char* function_name,
void* args,
void* result);
jit_error_t jit_apply_cognitive_hint(jit_context_t* ctx,
const char* hint_type,
const void* hint_data);
jit_error_t jit_get_stats(jit_context_t* ctx, jit_stats_t* stats);
const char* jit_get_error(jit_context_t* ctx);
void jit_destroy(jit_context_t* ctx);
bool jit_is_available(void);
const char* jit_get_version(void);
#ifdef __cplusplus
}
#endif
#endif