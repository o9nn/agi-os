/**
 * @file cognitive_grip_enhanced.h
 * @brief Enhanced Cognitive-Grip Integration Layer for AGI-OS
 * 
 * This header provides the enhanced unified abstraction layer that seamlessly
 * integrates CogNumach (microkernel), HurdCog (OS), OpenCog (cognitive framework),
 * and CogBolt (AI-powered IDE) into a cohesive cognitive operating system.
 * 
 * @author AGI-OS Development Team
 * @date December 12, 2025
 */

#ifndef COGNITIVE_GRIP_ENHANCED_H
#define COGNITIVE_GRIP_ENHANCED_H

#include <string>
#include <memory>
#include <vector>
#include <map>
#include <functional>

// Forward declarations for OpenCog components
namespace opencog {
    class AtomSpace;
    class Handle;
}

namespace agi_os {
namespace cognitive_grip {

/**
 * @brief Integration status enumeration
 */
enum class IntegrationStatus {
    UNINITIALIZED,
    INITIALIZING,
    READY,
    ERROR,
    DEGRADED
};

/**
 * @brief Layer identification
 */
enum class CognitiveLayer {
    MICROKERNEL,    // CogNumach
    OS,             // HurdCog
    COGNITIVE,      // OpenCog
    IDE,            // CogBolt
    INTEGRATION     // Cognitive-Grip
};

/**
 * @brief Enhanced Cognitive-Grip Manager
 * 
 * Provides unified access to all AGI-OS layers with seamless cognitive synergy
 */
class CognitiveGripEnhanced {
public:
    /**
     * @brief Get singleton instance
     */
    static CognitiveGripEnhanced& getInstance();

    /**
     * @brief Initialize all cognitive layers
     * @param config Configuration parameters
     * @return true if initialization successful
     */
    bool initialize(const std::map<std::string, std::string>& config);

    /**
     * @brief Shutdown all cognitive layers
     */
    void shutdown();

    /**
     * @brief Get integration status
     */
    IntegrationStatus getStatus() const;

    /**
     * @brief Check if a specific layer is available
     */
    bool isLayerAvailable(CognitiveLayer layer) const;

    // ========================================================================
    // Microkernel Integration (CogNumach)
    // ========================================================================

    /**
     * @brief Register microkernel state in AtomSpace
     * @param state_type Type of kernel state (memory, process, IPC, etc.)
     * @param state_data State data to register
     * @return Handle to the registered state atom
     */
    opencog::Handle registerKernelState(
        const std::string& state_type,
        const std::map<std::string, std::string>& state_data
    );

    /**
     * @brief Query microkernel state from AtomSpace
     * @param query_pattern Pattern matching query
     * @return Vector of matching state handles
     */
    std::vector<opencog::Handle> queryKernelState(
        const std::string& query_pattern
    );

    /**
     * @brief Apply cognitive scheduling policy
     * @param policy_name Name of the scheduling policy
     * @param parameters Policy parameters
     * @return true if policy applied successfully
     */
    bool applyCognitiveScheduling(
        const std::string& policy_name,
        const std::map<std::string, double>& parameters
    );

    // ========================================================================
    // Operating System Integration (HurdCog)
    // ========================================================================

    /**
     * @brief Register translator state in AtomSpace
     * @param translator_name Name of the Hurd translator
     * @param translator_data Translator state data
     * @return Handle to the registered translator atom
     */
    opencog::Handle registerTranslatorState(
        const std::string& translator_name,
        const std::map<std::string, std::string>& translator_data
    );

    /**
     * @brief Query filesystem semantically
     * @param semantic_query Natural language or pattern-based query
     * @return Vector of matching file/resource handles
     */
    std::vector<opencog::Handle> querySemanticFilesystem(
        const std::string& semantic_query
    );

    /**
     * @brief Register IPC message in AtomSpace for cognitive processing
     * @param message_type Type of IPC message
     * @param message_data Message data
     * @return Handle to the registered message atom
     */
    opencog::Handle registerIPCMessage(
        const std::string& message_type,
        const std::map<std::string, std::string>& message_data
    );

    // ========================================================================
    // Cognitive Framework Integration (OpenCog)
    // ========================================================================

    /**
     * @brief Get the main AtomSpace instance
     */
    std::shared_ptr<opencog::AtomSpace> getAtomSpace();

    /**
     * @brief Execute PLN inference on a query
     * @param query_handle Handle to the query atom
     * @param inference_steps Number of inference steps
     * @return Vector of inferred handles
     */
    std::vector<opencog::Handle> executePLNInference(
        opencog::Handle query_handle,
        int inference_steps = 100
    );

    /**
     * @brief Apply attention allocation using ECAN
     * @param resource_type Type of resource (CPU, memory, I/O)
     * @param allocation_policy Attention-based allocation policy
     * @return true if allocation successful
     */
    bool applyAttentionAllocation(
        const std::string& resource_type,
        const std::string& allocation_policy
    );

    /**
     * @brief Mine patterns from system behavior
     * @param behavior_type Type of behavior to mine
     * @param min_support Minimum support threshold
     * @return Vector of discovered pattern handles
     */
    std::vector<opencog::Handle> mineSystemPatterns(
        const std::string& behavior_type,
        double min_support = 0.1
    );

    // ========================================================================
    // IDE Integration (CogBolt)
    // ========================================================================

    /**
     * @brief Register code representation in AtomSpace
     * @param code_path Path to code file
     * @param code_ast Abstract syntax tree representation
     * @return Handle to the registered code atom
     */
    opencog::Handle registerCodeRepresentation(
        const std::string& code_path,
        const std::string& code_ast
    );

    /**
     * @brief Get AI-assisted code completion suggestions
     * @param context Current code context
     * @param cursor_position Cursor position in the code
     * @return Vector of completion suggestions
     */
    std::vector<std::string> getCodeCompletions(
        const std::string& context,
        int cursor_position
    );

    /**
     * @brief Analyze code using PLN reasoning
     * @param code_handle Handle to code representation
     * @param analysis_type Type of analysis (complexity, bugs, optimization)
     * @return Analysis results
     */
    std::map<std::string, std::string> analyzeCode(
        opencog::Handle code_handle,
        const std::string& analysis_type
    );

    /**
     * @brief Optimize code using pattern mining and reasoning
     * @param code_handle Handle to code representation
     * @return Optimized code suggestions
     */
    std::vector<std::string> optimizeCode(
        opencog::Handle code_handle
    );

    // ========================================================================
    // Cross-Layer Integration
    // ========================================================================

    /**
     * @brief Execute cross-layer cognitive query
     * @param query Natural language or pattern-based query
     * @param layers Layers to query (empty = all layers)
     * @return Query results from all layers
     */
    std::map<CognitiveLayer, std::vector<opencog::Handle>> executeCrossLayerQuery(
        const std::string& query,
        const std::vector<CognitiveLayer>& layers = {}
    );

    /**
     * @brief Register cross-layer event for cognitive processing
     * @param event_type Type of event
     * @param source_layer Source layer of the event
     * @param event_data Event data
     * @return Handle to the registered event atom
     */
    opencog::Handle registerCrossLayerEvent(
        const std::string& event_type,
        CognitiveLayer source_layer,
        const std::map<std::string, std::string>& event_data
    );

    /**
     * @brief Apply unified cognitive policy across all layers
     * @param policy_name Name of the policy
     * @param parameters Policy parameters
     * @return true if policy applied successfully
     */
    bool applyUnifiedCognitivePolicy(
        const std::string& policy_name,
        const std::map<std::string, std::string>& parameters
    );

    /**
     * @brief Get system-wide cognitive metrics
     * @return Map of metric names to values
     */
    std::map<std::string, double> getCognitiveMetrics();

    /**
     * @brief Register callback for cognitive events
     * @param event_type Type of event to listen for
     * @param callback Callback function
     */
    void registerEventCallback(
        const std::string& event_type,
        std::function<void(const std::map<std::string, std::string>&)> callback
    );

private:
    // Singleton pattern
    CognitiveGripEnhanced();
    ~CognitiveGripEnhanced();
    CognitiveGripEnhanced(const CognitiveGripEnhanced&) = delete;
    CognitiveGripEnhanced& operator=(const CognitiveGripEnhanced&) = delete;

    // Internal state
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};

/**
 * @brief Convenience function to get Cognitive-Grip instance
 */
inline CognitiveGripEnhanced& getCognitiveGrip() {
    return CognitiveGripEnhanced::getInstance();
}

} // namespace cognitive_grip
} // namespace agi_os

#endif // COGNITIVE_GRIP_ENHANCED_H
