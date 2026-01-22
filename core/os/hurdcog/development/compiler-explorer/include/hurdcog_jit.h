#ifndef HURDCOG_JIT_H
#define HURDCOG_JIT_H
#include "../src/jit_compiler.h"
#define HURDCOG_JIT_DEFAULT_CONFIG() \
{ \
.optimization_level = 2, \
.enable_cognitive = true, \
.enable_logging = true, \
.target_arch = "native", \
.log_file = NULL \
}
#define HURDCOG_JIT_MICROKERNEL_CONFIG() \
{ \
.optimization_level = 3, \
.enable_cognitive = true, \
.enable_logging = false, \
.target_arch = "native", \
.log_file = "/var/log/hurdcog-jit.log" \
}
static inline jit_error_t hurdcog_jit_quick_execute(const char* source_code,
const char* function_name,
void* result) {
jit_context_t* ctx = jit_init(NULL);
if (!ctx) return JIT_ERROR_INIT;
jit_error_t compile_result = jit_compile(ctx, source_code, function_name, NULL);
if (compile_result != JIT_SUCCESS) {
jit_destroy(ctx);
return compile_result;
}
jit_error_t exec_result = jit_execute(ctx, function_name, NULL, result);
jit_destroy(ctx);
return exec_result;
}
static inline jit_context_t* hurdcog_jit_init_microkernel(void) {
jit_config_t config = HURDCOG_JIT_MICROKERNEL_CONFIG();
return jit_init(&config);
}
#endif