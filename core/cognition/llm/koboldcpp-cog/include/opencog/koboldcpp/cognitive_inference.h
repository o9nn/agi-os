/**
 * @file cognitive_inference.h
 * @brief Cognitive inference engine combining LLM with AtomSpace reasoning
 *
 * Orchestrates the full cognitive inference pipeline:
 *   1. Extract relevant AtomSpace context (ECAN/PLN-guided)
 *   2. Build structured prompt with context
 *   3. Execute LLM inference via KoboldCpp
 *   4. Parse and store results back into AtomSpace
 *   5. Update attention values based on inference results
 */

#ifndef _OPENCOG_KOBOLDCPP_COGNITIVE_INFERENCE_H
#define _OPENCOG_KOBOLDCPP_COGNITIVE_INFERENCE_H

#include <string>
#include <vector>
#include <memory>
#include <functional>

#include "koboldcpp_client.h"
#include "atomspace_context.h"
#include "prompt_builder.h"

namespace opencog {
namespace koboldcpp {

/**
 * Inference mode determining how AtomSpace and LLM interact.
 */
enum class InferenceMode {
    QUERY,           ///< Answer a question using AtomSpace context
    GENERATE,        ///< Generate content from AtomSpace seeds
    REASON,          ///< Use LLM to assist PLN reasoning
    CLASSIFY,        ///< Classify atoms using LLM
    EXTRACT,         ///< Extract structured knowledge from text
    CONVERSE         ///< Multi-turn conversation with memory
};

/**
 * Result of a cognitive inference operation.
 */
struct CognitiveInferenceResult {
    std::string response_text;
    GenerationResult raw_result;
    int atoms_created = 0;      ///< New atoms added to AtomSpace
    int atoms_updated = 0;      ///< Existing atoms updated
    float confidence = 0.0f;    ///< Estimated confidence
    std::string inference_mode;
};

/**
 * @class CognitiveInference
 * @brief Full cognitive inference pipeline combining LLM + AtomSpace.
 *
 * This is the primary interface for cognitive LLM operations in AGI-OS.
 * It manages the lifecycle of inference requests, from context extraction
 * through prompt construction, LLM execution, and result integration.
 */
class CognitiveInference {
public:
    /**
     * Construct with a KoboldCpp client.
     * @param client Shared pointer to KoboldCpp client
     * @param context_config AtomSpace context extraction config
     */
    CognitiveInference(
        std::shared_ptr<KoboldCppClient> client,
        const ContextConfig& context_config = {},
        const PromptTemplate& prompt_template = {}
    );
    ~CognitiveInference();

    /**
     * Execute a cognitive inference request.
     * @param query The user query or task description
     * @param mode Inference mode
     * @param params Generation parameters
     * @return Cognitive inference result
     */
    CognitiveInferenceResult infer(
        const std::string& query,
        InferenceMode mode = InferenceMode::QUERY,
        const GenerationParams& params = {}
    ) const;

    /**
     * Execute inference with explicit context atoms.
     * @param query The query
     * @param context_atom_ids Specific atoms to use as context
     * @param mode Inference mode
     * @return Cognitive inference result
     */
    CognitiveInferenceResult infer_with_context(
        const std::string& query,
        const std::vector<std::string>& context_atom_ids,
        InferenceMode mode = InferenceMode::QUERY
    ) const;

    /**
     * Multi-turn conversation with AtomSpace memory.
     * @param message New user message
     * @return Response with updated conversation state
     */
    CognitiveInferenceResult converse(const std::string& message);

    /** Reset conversation state. */
    void reset_conversation();

    /** Get the underlying KoboldCpp client. */
    std::shared_ptr<KoboldCppClient> client() const;

    /** Check if the inference engine is ready. */
    bool is_ready() const;

private:
    struct Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace koboldcpp
} // namespace opencog

#endif // _OPENCOG_KOBOLDCPP_COGNITIVE_INFERENCE_H
