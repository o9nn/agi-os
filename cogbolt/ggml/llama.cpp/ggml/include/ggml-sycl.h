#pragma once
#include "ggml.h"
#include "ggml-backend.h"
#define GGML_SYCL_NAME "SYCL"
#define GGML_SYCL_MAX_DEVICES 48
#ifdef __cplusplus
extern "C" {
#endif
GGML_BACKEND_API ggml_backend_t ggml_backend_sycl_init(int device);
GGML_BACKEND_API bool ggml_backend_is_sycl(ggml_backend_t backend);
GGML_BACKEND_API ggml_backend_buffer_type_t ggml_backend_sycl_buffer_type(int device);
GGML_BACKEND_API ggml_backend_buffer_type_t ggml_backend_sycl_split_buffer_type(const float * tensor_split);
GGML_BACKEND_API ggml_backend_buffer_type_t ggml_backend_sycl_host_buffer_type(void);
GGML_BACKEND_API void ggml_backend_sycl_print_sycl_devices(void);
GGML_BACKEND_API void ggml_backend_sycl_get_gpu_list(int *id_list, int max_len);
GGML_BACKEND_API void ggml_backend_sycl_get_device_description(int device,
char *description,
size_t description_size);
GGML_BACKEND_API int ggml_backend_sycl_get_device_count();
GGML_BACKEND_API void ggml_backend_sycl_get_device_memory(int device, size_t *free, size_t *total);
GGML_BACKEND_API ggml_backend_reg_t ggml_backend_sycl_reg(void);
#ifdef __cplusplus
}
#endif