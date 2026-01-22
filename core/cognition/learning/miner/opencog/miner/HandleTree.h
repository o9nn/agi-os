#ifndef OPENCOG_HANDLETREE_H_
#define OPENCOG_HANDLETREE_H_
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/miner/tree.h>
namespace opencog {
typedef tree<Handle> HandleTree;
typedef tree<HandleMap> HandleMapTree;
typedef std::map<Handle, HandleTree> HandleHandleTreeMap;
bool content_eq(const HandleTree& htl, const HandleTree& htr);
bool content_eq(HandleTree::iterator itl, HandleTree::iterator itr);
bool content_contains(const HandleTree& ht, const Handle& h);
HandleTree merge_patterns(const std::initializer_list<HandleTree>&);
bool all_nodes_in(const HandleSet& cash, HandleTree::iterator it);
std::string oc_to_string(const HandleTree& ht,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleMapTree& hmt,
const std::string& indent=empty_string);
std::string oc_to_string(const HandleHandleTreeMap& hhtm,
const std::string& indent=empty_string);
}
#endif