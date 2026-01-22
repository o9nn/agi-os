#ifndef _COMBO_ASSUMPTION_H
#define _COMBO_ASSUMPTION_H
#include <opencog/asmoses/combo/combo/vertex.h>
namespace opencog { namespace combo {
void insert_assumption(combo_tree& tr, combo_tree::iterator assum_it);
bool find_vertices_in_assumptions(const combo_tree& tr, vertex v,
std::vector<combo_tree::iterator>& res);
bool equal_assumptions(const combo_tree& tr1, const combo_tree& tr2);
void delete_all_assumptions(combo_tree& tr);
}}
#endif