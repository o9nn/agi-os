#ifndef SHADERC_ENV_H_
#define SHADERC_ENV_H_
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef enum {
shaderc_target_env_vulkan,
shaderc_target_env_opengl,
shaderc_target_env_opengl_compat,
shaderc_target_env_webgpu,
shaderc_target_env_default = shaderc_target_env_vulkan
} shaderc_target_env;
typedef enum {
shaderc_env_version_vulkan_1_0 = ((1u << 22)),
shaderc_env_version_vulkan_1_1 = ((1u << 22) | (1 << 12)),
shaderc_env_version_vulkan_1_2 = ((1u << 22) | (2 << 12)),
shaderc_env_version_vulkan_1_3 = ((1u << 22) | (3 << 12)),
shaderc_env_version_vulkan_1_4 = ((1u << 22) | (4 << 12)),
shaderc_env_version_opengl_4_5 = 450,
shaderc_env_version_webgpu,
} shaderc_env_version;
typedef enum {
shaderc_spirv_version_1_0 = 0x010000u,
shaderc_spirv_version_1_1 = 0x010100u,
shaderc_spirv_version_1_2 = 0x010200u,
shaderc_spirv_version_1_3 = 0x010300u,
shaderc_spirv_version_1_4 = 0x010400u,
shaderc_spirv_version_1_5 = 0x010500u,
shaderc_spirv_version_1_6 = 0x010600u
} shaderc_spirv_version;
#ifdef __cplusplus
}
#endif
#endif