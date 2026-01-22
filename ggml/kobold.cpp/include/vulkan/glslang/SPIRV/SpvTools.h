#pragma once
#ifndef GLSLANG_SPV_TOOLS_H
#define GLSLANG_SPV_TOOLS_H
#if ENABLE_OPT
#include <vector>
#include <ostream>
#include <unordered_set>
#include "spirv-tools/libspirv.h"
#endif
#include "glslang/MachineIndependent/Versions.h"
#include "glslang/Include/visibility.h"
#include "GlslangToSpv.h"
#include "Logger.h"
namespace glslang {
#if ENABLE_OPT
class TIntermediate;
GLSLANG_EXPORT spv_target_env MapToSpirvToolsEnv(const SpvVersion& spvVersion, spv::SpvBuildLogger* logger);
GLSLANG_EXPORT spv_target_env MapToSpirvToolsEnv(const glslang::TIntermediate& intermediate, spv::SpvBuildLogger* logger);
GLSLANG_EXPORT void SpirvToolsDisassemble(std::ostream& out, const std::vector<unsigned int>& spirv);
GLSLANG_EXPORT void SpirvToolsDisassemble(std::ostream& out, const std::vector<unsigned int>& spirv,
spv_target_env requested_context);
GLSLANG_EXPORT void SpirvToolsValidate(const glslang::TIntermediate& intermediate, std::vector<unsigned int>& spirv,
spv::SpvBuildLogger*, bool prelegalization);
GLSLANG_EXPORT void SpirvToolsTransform(const glslang::TIntermediate& intermediate, std::vector<unsigned int>& spirv,
spv::SpvBuildLogger*, const SpvOptions*);
GLSLANG_EXPORT void SpirvToolsEliminateDeadInputComponents(spv_target_env target_env, std::vector<unsigned int>& spirv,
spv::SpvBuildLogger*);
GLSLANG_EXPORT bool SpirvToolsAnalyzeDeadOutputStores(spv_target_env target_env, std::vector<unsigned int>& spirv,
std::unordered_set<uint32_t>* live_locs,
std::unordered_set<uint32_t>* live_builtins,
spv::SpvBuildLogger*);
GLSLANG_EXPORT void SpirvToolsEliminateDeadOutputStores(spv_target_env target_env, std::vector<unsigned int>& spirv,
std::unordered_set<uint32_t>* live_locs,
std::unordered_set<uint32_t>* live_builtins,
spv::SpvBuildLogger*);
GLSLANG_EXPORT void SpirvToolsStripDebugInfo(const glslang::TIntermediate& intermediate,
std::vector<unsigned int>& spirv, spv::SpvBuildLogger*);
#endif
}
#endif