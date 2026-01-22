#ifndef _REDUCT_BRANCH_RULES_H
#define _REDUCT_BRANCH_RULES_H
#include "reduct.h"
namespace opencog { namespace reduct {
struct reduce_cond_arg : public crule<reduce_cond_arg>
{
int reduct_effort;
const vertex_set &ignore_ops;
reduce_cond_arg(int effort, const vertex_set &igop)
: crule<reduce_cond_arg>::crule("reduce_cond_arg"),
reduct_effort(effort), ignore_ops(igop) {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_cond_else : public crule<reduce_cond_else>
{
reduce_cond_else()
: crule<reduce_cond_else>::crule("reduce_cond_else") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_cond_adjacent : public crule<reduce_cond_adjacent>
{
reduce_cond_adjacent()
: crule<reduce_cond_adjacent>::crule("reduce_cond_adjacent") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_cond_const : public crule<reduce_cond_const>
{
reduce_cond_const()
: crule<reduce_cond_const>::crule("reduce_cond_const") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
}
}
#endif