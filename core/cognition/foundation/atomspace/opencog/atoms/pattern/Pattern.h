#ifndef _OPENCOG_PATTERN_H
#define _OPENCOG_PATTERN_H
#include <map>
#include <set>
#include <stack>
#include <unordered_map>
#include <vector>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/pattern/PatternTerm.h>
namespace opencog {
struct Pattern
{
typedef std::unordered_multimap<Handle, PatternTermPtr> ConnectMap;
typedef std::pair<Handle, PatternTermPtr> AtomInClausePair;
typedef std::map<AtomInClausePair, PatternTermSeq> ConnectTermMap;
Pattern() : group_min_size(0), group_max_size(-1), have_evaluatables(false) {}
std::string redex_name;
Handle           body;
PatternTermSeq   pmandatory;
PatternTermSeq absents;
PatternTermSeq always;
PatternTermSeq grouping;
long group_min_size;
long group_max_size;
bool have_evaluatables;
HandleSet defined_terms;
HandleSet cacheable_clauses;
std::map<PatternTermPtr, HandleSeq> clause_variables;
ConnectMap       connectivity_map;
ConnectTermMap   connected_terms_map;
std::string to_string(const std::string& indent) const;
};
std::string oc_to_string(const Pattern& pattern,
const std::string& indent=empty_string);
}
#endif