/**
 * @file atomspace_context.cpp
 * @brief AtomSpace context extraction for LLM inference
 */

#include "opencog/koboldcpp/atomspace_context.h"

#include <sstream>
#include <algorithm>

namespace opencog {
namespace koboldcpp {

struct AtomSpaceContextBuilder::Impl {
    ContextConfig config;

    Impl(const ContextConfig& cfg) : config(cfg) {}
};

AtomSpaceContextBuilder::AtomSpaceContextBuilder(const ContextConfig& config)
    : pimpl_(std::make_unique<Impl>(config)) {}

AtomSpaceContextBuilder::~AtomSpaceContextBuilder() = default;

AtomSpaceContext AtomSpaceContextBuilder::extract(const std::string& query) const {
    AtomSpaceContext ctx;

    // TODO: Connect to actual AtomSpace instance
    // For now, return placeholder context
    std::ostringstream ss;
    ss << "# AtomSpace Context for: " << query << "\n";
    ss << "# Strategy: ";
    switch (pimpl_->config.strategy) {
        case ContextStrategy::ATTENTION_WEIGHTED:
            ss << "ECAN Attention-Weighted"; break;
        case ContextStrategy::RECENCY_BASED:
            ss << "Recency-Based"; break;
        case ContextStrategy::TYPE_FILTERED:
            ss << "Type-Filtered"; break;
        case ContextStrategy::PLN_RELEVANT:
            ss << "PLN-Relevant"; break;
        case ContextStrategy::FULL_SUBGRAPH:
            ss << "Full Subgraph"; break;
    }
    ss << "\n# Max atoms: " << pimpl_->config.max_atoms << "\n";

    ctx.text_representation = ss.str();
    ctx.estimated_tokens = static_cast<int>(ctx.text_representation.size() / 4);
    ctx.atom_count = 0;

    return ctx;
}

AtomSpaceContext AtomSpaceContextBuilder::extract_around(
    const std::vector<std::string>& atom_ids) const {
    AtomSpaceContext ctx;
    ctx.atom_ids = atom_ids;
    ctx.atom_count = static_cast<int>(atom_ids.size());

    // TODO: Extract subgraph around given atoms
    std::ostringstream ss;
    ss << "# Context around " << atom_ids.size() << " focus atoms\n";
    for (const auto& id : atom_ids) {
        ss << "# - " << id << "\n";
    }

    ctx.text_representation = ss.str();
    ctx.estimated_tokens = static_cast<int>(ctx.text_representation.size() / 4);

    return ctx;
}

std::string AtomSpaceContextBuilder::serialize_atoms(
    const std::vector<std::string>& atom_ids) const {
    // TODO: Serialize actual atoms to natural language
    std::ostringstream ss;
    for (const auto& id : atom_ids) {
        ss << "Atom: " << id << "\n";
    }
    return ss.str();
}

void AtomSpaceContextBuilder::set_config(const ContextConfig& config) {
    pimpl_->config = config;
}

} // namespace koboldcpp
} // namespace opencog
