/**
 * @file prompt_builder.cpp
 * @brief Structured prompt construction from AtomSpace context
 */

#include "opencog/koboldcpp/prompt_builder.h"

#include <sstream>

namespace opencog {
namespace koboldcpp {

struct PromptBuilder::Impl {
    PromptTemplate tmpl;
    Impl(const PromptTemplate& t) : tmpl(t) {}
};

PromptBuilder::PromptBuilder(const PromptTemplate& tmpl)
    : pimpl_(std::make_unique<Impl>(tmpl)) {}

PromptBuilder::~PromptBuilder() = default;

std::string PromptBuilder::build_completion(const AtomSpaceContext& context,
                                             const std::string& query) const {
    std::ostringstream ss;

    // System prompt
    ss << pimpl_->tmpl.system_prompt << "\n\n";

    // Context section (if available)
    if (!context.text_representation.empty()) {
        ss << pimpl_->tmpl.context_prefix;
        ss << context.text_representation;
        ss << pimpl_->tmpl.context_suffix;
    }

    // Query
    ss << pimpl_->tmpl.query_prefix;
    ss << query;
    ss << pimpl_->tmpl.query_suffix;

    return ss.str();
}

std::vector<ChatMessage> PromptBuilder::build_chat(
    const AtomSpaceContext& context,
    const std::string& query,
    const std::vector<ChatMessage>& history) const {

    std::vector<ChatMessage> messages;

    // System message with context
    std::ostringstream system;
    system << pimpl_->tmpl.system_prompt;
    if (!context.text_representation.empty()) {
        system << "\n\n" << pimpl_->tmpl.context_prefix;
        system << context.text_representation;
        system << pimpl_->tmpl.context_suffix;
    }
    messages.push_back({"system", system.str()});

    // Conversation history
    for (const auto& msg : history) {
        messages.push_back(msg);
    }

    // Current query
    messages.push_back({"user", query});

    return messages;
}

int PromptBuilder::estimate_tokens(const std::string& text) {
    // Rough estimate: ~4 characters per token for English
    return static_cast<int>(text.size() / 4);
}

void PromptBuilder::set_template(const PromptTemplate& tmpl) {
    pimpl_->tmpl = tmpl;
}

} // namespace koboldcpp
} // namespace opencog
