#ifndef __APPLE__
#ifndef __GPU_INFO_NVML_H__
#define __GPU_INFO_NVML_H__
#include "gpu_info.h"
typedef enum nvmlReturn_enum {
NVML_SUCCESS = 0,
} nvmlReturn_t;
typedef void *nvmlDevice_t;
typedef struct nvmlMemory_st {
unsigned long long total;
unsigned long long free;
unsigned long long used;
} nvmlMemory_t;
typedef enum nvmlBrandType_enum
{
NVML_BRAND_UNKNOWN          = 0,
} nvmlBrandType_t;
typedef struct nvml_handle {
void *handle;
uint16_t verbose;
nvmlReturn_t (*nvmlInit_v2)(void);
nvmlReturn_t (*nvmlShutdown)(void);
nvmlReturn_t (*nvmlDeviceGetHandleByUUID)(const char *, nvmlDevice_t *);
nvmlReturn_t (*nvmlDeviceGetMemoryInfo)(nvmlDevice_t, nvmlMemory_t *);
} nvml_handle_t;
typedef struct nvml_init_resp {
char *err;
nvml_handle_t ch;
} nvml_init_resp_t;
typedef struct nvml_compute_capability {
char *err;
int major;
int minor;
} nvml_compute_capability_t;
void nvml_init(char *nvml_lib_path, nvml_init_resp_t *resp);
void nvml_get_free(nvml_handle_t ch, char *uuid, uint64_t *free, uint64_t *total, uint64_t *used);
void nvml_release(nvml_handle_t ch);
#endif
#endif