#ifndef GLSLANG_SPIRV_LOGGER_H
#define GLSLANG_SPIRV_LOGGER_H
#include <string>
#include <vector>
#include "glslang/Include/visibility.h"
namespace spv {
class GLSLANG_EXPORT SpvBuildLogger {
public:
SpvBuildLogger() {}
void tbdFunctionality(const std::string& f);
void missingFunctionality(const std::string& f);
void warning(const std::string& w) { warnings.push_back(w); }
void error(const std::string& e) { errors.push_back(e); }
std::string getAllMessages() const;
private:
SpvBuildLogger(const SpvBuildLogger&);
std::vector<std::string> tbdFeatures;
std::vector<std::string> missingFeatures;
std::vector<std::string> warnings;
std::vector<std::string> errors;
};
}
#endif