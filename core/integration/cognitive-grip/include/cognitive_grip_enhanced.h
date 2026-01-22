#ifndef COGNITIVE_GRIP_ENHANCED_H
#define COGNITIVE_GRIP_ENHANCED_H
#include <string>
#include <memory>
#include <vector>
#include <map>
#include <functional>
namespace opencog {
class AtomSpace;
class Handle;
}
namespace agi_os {
namespace cognitive_grip {
enum class IntegrationStatus {
UNINITIALIZED,
INITIALIZING,
READY,
ERROR,
DEGRADED
};
enum class CognitiveLayer {
MICROKERNEL,
OS,
COGNITIVE,
IDE,
INTEGRATION
};
class CognitiveGripEnhanced {
public:
static CognitiveGripEnhanced& getInstance();
bool initialize(const std::map<std::string, std::string>& config);
void shutdown();
IntegrationStatus getStatus() const;
bool isLayerAvailable(CognitiveLayer layer) const;
opencog::Handle registerKernelState(
const std::string& state_type,
const std::map<std::string, std::string>& state_data
);
std::vector<opencog::Handle> queryKernelState(
const std::string& query_pattern
);
bool applyCognitiveScheduling(
const std::string& policy_name,
const std::map<std::string, double>& parameters
);
opencog::Handle registerTranslatorState(
const std::string& translator_name,
const std::map<std::string, std::string>& translator_data
);
std::vector<opencog::Handle> querySemanticFilesystem(
const std::string& semantic_query
);
opencog::Handle registerIPCMessage(
const std::string& message_type,
const std::map<std::string, std::string>& message_data
);
std::shared_ptr<opencog::AtomSpace> getAtomSpace();
std::vector<opencog::Handle> executePLNInference(
opencog::Handle query_handle,
int inference_steps = 100
);
bool applyAttentionAllocation(
const std::string& resource_type,
const std::string& allocation_policy
);
std::vector<opencog::Handle> mineSystemPatterns(
const std::string& behavior_type,
double min_support = 0.1
);
opencog::Handle registerCodeRepresentation(
const std::string& code_path,
const std::string& code_ast
);
std::vector<std::string> getCodeCompletions(
const std::string& context,
int cursor_position
);
std::map<std::string, std::string> analyzeCode(
opencog::Handle code_handle,
const std::string& analysis_type
);
std::vector<std::string> optimizeCode(
opencog::Handle code_handle
);
std::map<CognitiveLayer, std::vector<opencog::Handle>> executeCrossLayerQuery(
const std::string& query,
const std::vector<CognitiveLayer>& layers = {}
);
opencog::Handle registerCrossLayerEvent(
const std::string& event_type,
CognitiveLayer source_layer,
const std::map<std::string, std::string>& event_data
);
bool applyUnifiedCognitivePolicy(
const std::string& policy_name,
const std::map<std::string, std::string>& parameters
);
std::map<std::string, double> getCognitiveMetrics();
void registerEventCallback(
const std::string& event_type,
std::function<void(const std::map<std::string, std::string>&)> callback
);
private:
CognitiveGripEnhanced();
~CognitiveGripEnhanced();
CognitiveGripEnhanced(const CognitiveGripEnhanced&) = delete;
CognitiveGripEnhanced& operator=(const CognitiveGripEnhanced&) = delete;
struct Impl;
std::unique_ptr<Impl> pImpl;
};
inline CognitiveGripEnhanced& getCognitiveGrip() {
return CognitiveGripEnhanced::getInstance();
}
}
}
#endif