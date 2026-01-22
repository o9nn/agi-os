#ifndef _OPENCOG_PATTERN_UTILS_H
#define _OPENCOG_PATTERN_UTILS_H
#include <set>
#include <vector>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
#include "Pattern.h"
namespace opencog {
bool can_evaluate(const Handle& clause);
bool is_constant(const HandleSet& vars, const Handle& clause);
bool is_black_box(const Handle& clause);
void get_connected_components(const HandleSet& vars,
const HandleSeq& clauses,
HandleSeqSeq& compset,
HandleSetSeq& compvars);
void get_bridged_components(const HandleSet& vars,
const PatternTermSeq& clauses,
const PatternTermSeq& opts,
HandleSeqSeq& compset,
HandleSetSeq& compvars);
}
#endif