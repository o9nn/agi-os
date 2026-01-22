#ifndef _STAND_ALONE_RESOURCE_LIMITS_C_INCLUDED_
#define _STAND_ALONE_RESOURCE_LIMITS_C_INCLUDED_
#include "../Include/glslang_c_interface.h"
#include "../Include/visibility.h"
#ifdef __cplusplus
extern "C" {
#endif
GLSLANG_EXPORT glslang_resource_t* glslang_resource(void);
GLSLANG_EXPORT const glslang_resource_t* glslang_default_resource(void);
GLSLANG_EXPORT const char* glslang_default_resource_string();
GLSLANG_EXPORT void glslang_decode_resource_limits(glslang_resource_t* resources, char* config);
#ifdef __cplusplus
}
#endif
#endif