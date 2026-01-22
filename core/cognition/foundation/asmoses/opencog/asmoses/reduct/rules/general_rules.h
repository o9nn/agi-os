#ifndef _REDUCT_GENERAL_RULES_H
#define _REDUCT_GENERAL_RULES_H
#include <opencog/util/RandGen.h>
#include "opencog/asmoses/reduct/reduct/reduct.h"
namespace opencog { namespace reduct {
struct level : public crule<level>
{
level() : crule<level>::crule("level") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct eval_constants : public crule<eval_constants>
{
eval_constants() : crule<eval_constants>::crule("eval_constants") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reorder_commutative : public crule<reorder_commutative>
{
reorder_commutative()
: crule<reorder_commutative>::crule("reorder_commutative") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct remove_null_vertices : public crule<remove_null_vertices>
{
remove_null_vertices()
: crule<remove_null_vertices>::crule("remove_null_vertices") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct remove_all_assumptions : public crule<remove_all_assumptions>
{
remove_all_assumptions()
: crule<remove_all_assumptions>::crule("remove_all_assumptions") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
}
}
#endif