#pragma once
#include "ggml-backend.h"
#include "ggml.h"
#ifdef __cplusplus
extern "C" {
#endif
#define GGML_CANN_MAX_DEVICES 16
GGML_BACKEND_API ggml_backend_reg_t ggml_backend_cann_reg(void);
GGML_BACKEND_API ggml_backend_t ggml_backend_cann_init(int32_t device);
GGML_BACKEND_API bool ggml_backend_is_cann(ggml_backend_t backend);
GGML_BACKEND_API ggml_backend_buffer_type_t
ggml_backend_cann_buffer_type(int32_t device);
GGML_BACKEND_API int32_t ggml_backend_cann_get_device_count(void);
GGML_BACKEND_API ggml_backend_buffer_type_t ggml_backend_cann_host_buffer_type(void);
GGML_BACKEND_API void ggml_backend_cann_get_device_description(
int32_t device, char* description, size_t description_size);
GGML_BACKEND_API void ggml_backend_cann_get_device_memory(int32_t device,
size_t* free,
size_t* total);
#ifdef __cplusplus
}
#endif