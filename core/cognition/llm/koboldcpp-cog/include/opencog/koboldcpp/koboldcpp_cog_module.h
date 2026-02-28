/**
 * @file koboldcpp_cog_module.h
 * @brief CogServer module for KoboldCpp integration
 *
 * Registers KoboldCpp inference as a CogServer module, exposing
 * LLM capabilities through the CogServer network interface and
 * Scheme/Python bindings.
 */

#ifndef _OPENCOG_KOBOLDCPP_COG_MODULE_H
#define _OPENCOG_KOBOLDCPP_COG_MODULE_H

#include <string>
#include <memory>

namespace opencog {
namespace koboldcpp {

class CognitiveInference;

/**
 * @class KoboldCppCogModule
 * @brief CogServer module providing LLM inference via KoboldCpp.
 *
 * When loaded into CogServer, this module:
 *   - Connects to a KoboldCpp instance
 *   - Registers Scheme primitives for LLM inference
 *   - Registers Python bindings
 *   - Provides a cognitive agent for autonomous inference
 *   - Exposes REST API endpoints for LLM operations
 */
class KoboldCppCogModule {
public:
    /**
     * Initialize the module.
     * @param endpoint KoboldCpp server URL
     * @param auto_connect Attempt connection on init
     */
    explicit KoboldCppCogModule(
        const std::string& endpoint = "http://localhost:5001",
        bool auto_connect = true
    );
    ~KoboldCppCogModule();

    /** Module lifecycle. */
    void init();
    void run();
    void shutdown();

    /** Get the cognitive inference engine. */
    std::shared_ptr<CognitiveInference> inference() const;

    /** Module identification. */
    static const char* id() { return "opencog::koboldcpp::KoboldCppCogModule"; }

private:
    struct Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace koboldcpp
} // namespace opencog

#endif // _OPENCOG_KOBOLDCPP_COG_MODULE_H
