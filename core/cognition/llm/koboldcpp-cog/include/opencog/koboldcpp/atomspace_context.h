/**
 * @file atomspace_context.h
 * @brief AtomSpace context extraction for LLM inference
 *
 * Extracts relevant atoms from the AtomSpace to construct context
 * for LLM inference requests. Supports ECAN attention-weighted
 * selection and PLN-guided relevance filtering.
 */

#ifndef _OPENCOG_KOBOLDCPP_ATOMSPACE_CONTEXT_H
#define _OPENCOG_KOBOLDCPP_ATOMSPACE_CONTEXT_H

#include <string>
#include <vector>
#include <memory>

namespace opencog {
namespace koboldcpp {

/**
 * Context extraction strategy.
 */
enum class ContextStrategy {
    ATTENTION_WEIGHTED,  ///< Use ECAN attention values
    RECENCY_BASED,       ///< Most recently modified atoms
    TYPE_FILTERED,       ///< Filter by atom type
    PLN_RELEVANT,        ///< PLN-guided relevance
    FULL_SUBGRAPH        ///< Extract full subgraph around focus
};

/**
 * Configuration for context extraction.
 */
struct ContextConfig {
    ContextStrategy strategy = ContextStrategy::ATTENTION_WEIGHTED;
    int max_atoms = 100;          ///< Maximum atoms to include
    int max_context_tokens = 2048; ///< Approximate token budget
    float min_attention = 0.1f;    ///< Minimum attention threshold
    std::vector<std::string> focus_types;  ///< Atom types to prioritize
};

/**
 * Extracted context ready for prompt construction.
 */
struct AtomSpaceContext {
    std::string text_representation;  ///< Serialized context text
    int estimated_tokens = 0;         ///< Estimated token count
    int atom_count = 0;               ///< Number of atoms included
    std::vector<std::string> atom_ids; ///< IDs of included atoms
};

/**
 * @class AtomSpaceContextBuilder
 * @brief Extracts and serializes AtomSpace content for LLM context.
 *
 * Bridges the symbolic knowledge in AtomSpace with the natural language
 * interface of LLM inference. Supports multiple extraction strategies
 * to select the most relevant atoms for a given query.
 */
class AtomSpaceContextBuilder {
public:
    explicit AtomSpaceContextBuilder(const ContextConfig& config = {});
    ~AtomSpaceContextBuilder();

    /**
     * Extract context from AtomSpace for a given query.
     * @param query Natural language query to contextualize
     * @return Extracted context
     */
    AtomSpaceContext extract(const std::string& query) const;

    /**
     * Extract context around specific atom IDs.
     * @param atom_ids Focus atoms to build context around
     * @return Extracted context
     */
    AtomSpaceContext extract_around(const std::vector<std::string>& atom_ids) const;

    /**
     * Serialize atoms to natural language text.
     * @param atom_ids Atoms to serialize
     * @return Natural language representation
     */
    std::string serialize_atoms(const std::vector<std::string>& atom_ids) const;

    /** Update configuration. */
    void set_config(const ContextConfig& config);

private:
    struct Impl;
    std::unique_ptr<Impl> pimpl_;
};

} // namespace koboldcpp
} // namespace opencog

#endif // _OPENCOG_KOBOLDCPP_ATOMSPACE_CONTEXT_H
