#ifndef _ATOMSPACE_ACCELERATOR_PATTERN_MINER_H
#define _ATOMSPACE_ACCELERATOR_PATTERN_MINER_H
#include <memory>
#include <string>
#include <vector>
namespace atomspace_accelerator {
struct CognitivePattern {
std::string patternId;
std::string description;
double support;
double confidence;
size_t frequency;
std::vector<std::string> components;
};
class PatternMiner {
public:
PatternMiner();
~PatternMiner();
std::vector<CognitivePattern> minePatterns(const std::string& data);
std::vector<CognitivePattern> mineFrequentPatterns(double minSupport);
std::vector<std::pair<std::string, std::string>> mineAssociations(double minConfidence);
void addData(const std::string& data);
std::string getMiningStats() const;
void setMinimumSupport(double support);
void setMinimumConfidence(double confidence);
private:
class Impl;
std::unique_ptr<Impl> pImpl;
};
}
#endif