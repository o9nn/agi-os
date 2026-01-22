#ifndef _STAND_ALONE_RESOURCE_LIMITS_INCLUDED_
#define _STAND_ALONE_RESOURCE_LIMITS_INCLUDED_
#include <string>
#include "../Include/ResourceLimits.h"
#include "../Include/visibility.h"
GLSLANG_EXPORT extern TBuiltInResource* GetResources();
GLSLANG_EXPORT extern const TBuiltInResource* GetDefaultResources();
GLSLANG_EXPORT std::string GetDefaultTBuiltInResourceString();
GLSLANG_EXPORT void DecodeResourceLimits(TBuiltInResource* resources, char* config);
#endif