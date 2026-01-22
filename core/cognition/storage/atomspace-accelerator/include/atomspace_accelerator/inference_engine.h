#ifndef _ATOMSPACE_ACCELERATOR_INFERENCE_ENGINE_H
#define _ATOMSPACE_ACCELERATOR_INFERENCE_ENGINE_H
#include <memory>
#include <string>
#include <vector>
namespace atomspace_accelerator {
class InferenceEngine {
public:
InferenceEngine();
~InferenceEngine();
bool initialize();
void shutdown();
std::string executeQuery(const std::string& query);
std::string getStatistics() const;
void optimizeInferencePaths();
bool isReady() const;
private:
class Impl;
std::unique_ptr<Impl> pImpl;
};
}
#endif