#ifndef COMBO_SIMILARITY_H_
#define COMBO_SIMILARITY_H_
#include <iostream>
#include <map>
#include <string>
#include <sstream>
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace combo {
typedef std::map<std::string, unsigned> tree_branch_vector;
tree_branch_vector tree_flatten(const combo_tree&);
tree_branch_vector tree_flatten(const std::string& str);
size_t tree_similarity(const combo_tree&, const combo_tree&);
size_t tree_similarity(const tree_branch_vector&, const tree_branch_vector&);
size_t tree_similarity(const std::string&, const std::string&);
std::ostream& operator<<(std::ostream&, const tree_branch_vector&);
std::string toString(const tree_branch_vector& tbv)
{
std::stringstream ss;
ss << tbv;
return ss.str();
}
}
}
#endif