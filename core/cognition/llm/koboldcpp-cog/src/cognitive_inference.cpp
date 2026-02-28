/**
 * @file cognitive_inference.cpp
 * @brief Cognitive inference pipeline combining LLM with AtomSpace
 */

#include "opencog/koboldcpp/cognitive_inference.h"

#include <sstream>

namespace opencog {
namespace koboldcpp {

struct CognitiveInference::Impl {
    std::shared_ptr<KoboldCppClient> client;
    AtomSpaceContextBuilder context_builder;
    PromptBuilder prompt_builder;
    std::vector<ChatMessage> conversation_history;

    Impl(std::shared_ptr<KoboldCppClient> c,
         const ContextConfig& cc,
         const PromptTemplate& pt)
        : client(std::move(c))
        , context_builder(cc)
        , prompt_builder(pt) {}
};

CognitiveInference::CognitiveInference(
    std::shared_ptr<KoboldCppClient> client,
    const ContextConfig& context_config,
    const PromptTemplate& prompt_template)
    : pimpl_(std::make_unique<Impl>(std::move(client), context_config, prompt_template)) {}

CognitiveInference::~CognitiveInference() = default;

CognitiveInferenceResult CognitiveInference::infer(
    const std::string& query,
    InferenceMode mode,
    const GenerationParams& params) const {

    CognitiveInferenceResult result;

    // Step 1: Extract AtomSpace context
    auto context = pimpl_->context_builder.extract(query);

    // Step 2: Build prompt
    std::string prompt = pimpl_->prompt_builder.build_completion(context, query);

    // Step 3: Execute inference
    if (mode == InferenceMode::CONVERSE) {
        auto messages = pimpl_->prompt_builder.build_chat(
            context, query, pimpl_->conversation_history);
        result.raw_result = pimpl_->client->chat(messages, params);
    } else {
        result.raw_result = pimpl_->client->generate(prompt, params);
    }

    // Step 4: Process result
    result.response_text = result.raw_result.text;
    result.confidence = result.raw_result.success ? 0.7f : 0.0f;

    switch (mode) {
        case InferenceMode::QUERY:
            result.inference_mode = "query"; break;
        case InferenceMode::GENERATE:
            result.inference_mode = "generate"; break;
        case InferenceMode::REASON:
            result.inference_mode = "reason"; break;
        case InferenceMode::CLASSIFY:
            result.inference_mode = "classify"; break;
        case InferenceMode::EXTRACT:
            result.inference_mode = "extract"; break;
        case InferenceMode::CONVERSE:
            result.inference_mode = "converse"; break;
    }

    // Step 5: TODO - Store results back into AtomSpace
    // TODO - Update ECAN attention values

    return result;
}

CognitiveInferenceResult CognitiveInference::infer_with_context(
    const std::string& query,
    const std::vector<std::string>& context_atom_ids,
    InferenceMode mode) const {

    CognitiveInferenceResult result;

    auto context = pimpl_->context_builder.extract_around(context_atom_ids);
    std::string prompt = pimpl_->prompt_builder.build_completion(context, query);

    result.raw_result = pimpl_->client->generate(prompt, {});
    result.response_text = result.raw_result.text;
    result.confidence = result.raw_result.success ? 0.7f : 0.0f;

    return result;
}

CognitiveInferenceResult CognitiveInference::converse(const std::string& message) {
    // Add user message to history
    pimpl_->conversation_history.push_back({"user", message});

    auto result = infer(message, InferenceMode::CONVERSE, {});

    // Add assistant response to history
    if (result.raw_result.success) {
        pimpl_->conversation_history.push_back(
            {"assistant", result.response_text});
    }

    return result;
}

void CognitiveInference::reset_conversation() {
    pimpl_->conversation_history.clear();
}

std::shared_ptr<KoboldCppClient> CognitiveInference::client() const {
    return pimpl_->client;
}

bool CognitiveInference::is_ready() const {
    return pimpl_->client && pimpl_->client->is_connected();
}

} // namespace koboldcpp
} // namespace opencog
