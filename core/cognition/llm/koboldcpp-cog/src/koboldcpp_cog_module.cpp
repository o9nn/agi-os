/**
 * @file koboldcpp_cog_module.cpp
 * @brief CogServer module for KoboldCpp integration
 */

#include "opencog/koboldcpp/koboldcpp_cog_module.h"
#include "opencog/koboldcpp/cognitive_inference.h"
#include "opencog/koboldcpp/koboldcpp_client.h"

#include <iostream>

namespace opencog {
namespace koboldcpp {

struct KoboldCppCogModule::Impl {
    std::string endpoint;
    bool auto_connect;
    std::shared_ptr<KoboldCppClient> client;
    std::shared_ptr<CognitiveInference> inference_engine;

    Impl(const std::string& ep, bool ac)
        : endpoint(ep), auto_connect(ac) {}
};

KoboldCppCogModule::KoboldCppCogModule(const std::string& endpoint,
                                         bool auto_connect)
    : pimpl_(std::make_unique<Impl>(endpoint, auto_connect)) {}

KoboldCppCogModule::~KoboldCppCogModule() = default;

void KoboldCppCogModule::init() {
    std::cout << "[KoboldCppCogModule] Initializing..." << std::endl;
    std::cout << "[KoboldCppCogModule] Endpoint: " << pimpl_->endpoint << std::endl;

    pimpl_->client = std::make_shared<KoboldCppClient>(pimpl_->endpoint);

    if (pimpl_->auto_connect) {
        if (pimpl_->client->is_connected()) {
            std::cout << "[KoboldCppCogModule] Connected to KoboldCpp" << std::endl;
            auto info = pimpl_->client->get_info();
            std::cout << "[KoboldCppCogModule] Model: " << info.model_name << std::endl;
        } else {
            std::cout << "[KoboldCppCogModule] KoboldCpp not available at "
                      << pimpl_->endpoint << std::endl;
            std::cout << "[KoboldCppCogModule] Will retry on first inference request"
                      << std::endl;
        }
    }

    pimpl_->inference_engine = std::make_shared<CognitiveInference>(
        pimpl_->client);

    // TODO: Register Scheme primitives
    // (cog-kobold-generate prompt max-tokens) -> string
    // (cog-kobold-chat messages) -> string
    // (cog-kobold-infer query mode) -> string
    // (cog-kobold-info) -> alist

    // TODO: Register Python bindings
    // opencog.koboldcpp.generate(prompt, max_tokens)
    // opencog.koboldcpp.chat(messages)
    // opencog.koboldcpp.infer(query, mode)

    std::cout << "[KoboldCppCogModule] Initialized" << std::endl;
}

void KoboldCppCogModule::run() {
    // Module is event-driven, nothing to do in run loop
}

void KoboldCppCogModule::shutdown() {
    std::cout << "[KoboldCppCogModule] Shutting down..." << std::endl;
    pimpl_->inference_engine.reset();
    pimpl_->client.reset();
}

std::shared_ptr<CognitiveInference> KoboldCppCogModule::inference() const {
    return pimpl_->inference_engine;
}

} // namespace koboldcpp
} // namespace opencog
