/**
 * @file prompt_builder.h
 * @brief Structured prompt construction from AtomSpace context
 *
 * Builds prompts for LLM inference using templates that incorporate
 * AtomSpace context, chat history, and task-specific instructions.
 */

#ifndef _OPENCOG_KOBOLDCPP_PROMPT_BUILDER_H
#define _OPENCOG_KOBOLDCPP_PROMPT_BUILDER_H

#include <string>
#include <vector>
#include <map>

#include "atomspace_context.h"
#include "koboldcpp_client.h"

namespace opencog {
namespace koboldcpp {

/**
 * Template for prompt construction.
 */
struct PromptTemplate {
    std::string system_prompt = "You are a cognitive agent with access to a knowledge graph.";
    std::string context_prefix = "## Knowledge Context\n";
    std::string context_suffix = "\n## End Context\n\n";
    std::string query_prefix = "## Query\n";
    std::string query_suffix = "\n\n## Response\n";
    int max_context_tokens = 2048;
};

/**
 * @class PromptBuilder
 * @brief Constructs structured prompts from AtomSpace context and queries.
 *
 * Supports multiple chat template formats (ChatML, Llama 3, Gemma)
 * and automatically adapts to the model loaded in KoboldCpp.
 */
class PromptBuilder {
public:
    explicit PromptBuilder(const PromptTemplate& tmpl = {});
    ~PromptBuilder();

    /**
     * Build a text completion prompt.
     * @param context AtomSpace context
     * @param query User query
     * @return Formatted prompt string
     */
    std::string build_completion(const AtomSpaceContext& context,
                                 const std::string& query) const;

    /**
     * Build chat messages for chat completion API.
     * @param context AtomSpace context
     * @param query User query
     * @param history Previous conversation messages
     * @return Vector of ChatMessage for the API
     */
    std::vector<ChatMessage> build_chat(
        const AtomSpaceContext& context,
        const std::string& query,
        const std::vector<ChatMessage>& history = {}
    ) const;

    /**
     * Estimate token count for a string.
     * @param text Input text
     * @return Approximate token count
     */
    static int estimate_tokens(const std::string& text);

    /** Update the prompt template. */
    void set_template(const PromptTemplate& tmpl);

private:
    struct Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace koboldcpp
} // namespace opencog

#endif // _OPENCOG_KOBOLDCPP_PROMPT_BUILDER_H
