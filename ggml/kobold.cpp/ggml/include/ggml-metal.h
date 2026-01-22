#pragma once
#include "ggml.h"
#include "ggml-backend.h"
#include <stddef.h>
#include <stdbool.h>
struct ggml_tensor;
struct ggml_cgraph;
#ifdef __cplusplus
extern "C" {
#endif
GGML_BACKEND_API ggml_backend_t ggml_backend_metal_init(void);
GGML_BACKEND_API bool ggml_backend_is_metal(ggml_backend_t backend);
GGML_DEPRECATED(
GGML_BACKEND_API ggml_backend_buffer_t ggml_backend_metal_buffer_from_ptr(void * data, size_t size, size_t max_size),
"obsoleted by the new device interface - https:
GGML_BACKEND_API void ggml_backend_metal_set_abort_callback(ggml_backend_t backend, ggml_abort_callback abort_callback, void * user_data);
GGML_BACKEND_API ggml_backend_buffer_type_t ggml_backend_metal_buffer_type(void);
GGML_BACKEND_API bool ggml_backend_metal_supports_family(ggml_backend_t backend, int family);
GGML_BACKEND_API void ggml_backend_metal_capture_next_compute(ggml_backend_t backend);
GGML_BACKEND_API ggml_backend_reg_t ggml_backend_metal_reg(void);
#ifdef __cplusplus
}
#endif