#pragma once
#include <string>
#include <vector>
#include "Logger.h"
#include "glslang/Include/visibility.h"
namespace glslang {
class TIntermediate;
struct SpvOptions {
bool generateDebugInfo {false};
bool stripDebugInfo {false};
bool disableOptimizer {true};
bool optimizeSize {false};
bool disassemble {false};
bool validate {false};
bool emitNonSemanticShaderDebugInfo {false};
bool emitNonSemanticShaderDebugSource{ false };
bool compileOnly{false};
bool optimizerAllowExpandedIDBound{false};
};
GLSLANG_EXPORT void GetSpirvVersion(std::string&);
GLSLANG_EXPORT int GetSpirvGeneratorVersion();
GLSLANG_EXPORT void GlslangToSpv(const glslang::TIntermediate& intermediate, std::vector<unsigned int>& spirv,
SpvOptions* options = nullptr);
GLSLANG_EXPORT void GlslangToSpv(const glslang::TIntermediate& intermediate, std::vector<unsigned int>& spirv,
spv::SpvBuildLogger* logger, SpvOptions* options = nullptr);
GLSLANG_EXPORT bool OutputSpvBin(const std::vector<unsigned int>& spirv, const char* baseName);
GLSLANG_EXPORT bool OutputSpvHex(const std::vector<unsigned int>& spirv, const char* baseName, const char* varName);
}