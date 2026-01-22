#ifndef _REDUCT_ACTION_RULES_H
#define _REDUCT_ACTION_RULES_H
#include "reduct.h"
#include <boost/logic/tribool.hpp>
namespace opencog { namespace reduct {
using namespace combo;
bool safe_bool(boost::tribool t);
boost::tribool get_action_result(const combo_tree& tr, combo_tree::iterator it);
void substitute_condition_init_instant(combo_tree& tr, combo_tree::iterator it,
combo_tree::iterator cond, vertex sub);
struct reduce_action_if : public crule<reduce_action_if> {
reduce_action_if() :
crule<reduce_action_if>::crule("reduce_action_if") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_action_action_if : public crule<reduce_action_action_if> {
reduce_action_action_if() :
crule<reduce_action_action_if>::crule("reduce_action_action_if") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_const_cond_action_if: public crule<reduce_const_cond_action_if> {
reduce_const_cond_action_if() :
crule<reduce_const_cond_action_if>::crule("reduce_const_cond_action_if") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_not_cond_action_boolean_if:
public crule<reduce_not_cond_action_boolean_if> {
reduce_not_cond_action_boolean_if() :
crule<reduce_not_cond_action_boolean_if>::crule("reduce_not_cond_action_boolean_if") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_const_action_seq : public crule<reduce_const_action_seq> {
reduce_const_action_seq () :
crule<reduce_const_action_seq>::crule("reduce_const_action_seq") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_empty_arg_seq : public crule<reduce_empty_arg_seq> {
reduce_empty_arg_seq () :
crule<reduce_empty_arg_seq>::crule("reduce_empty_arg_seq") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_double_action_not : public crule<reduce_double_action_not> {
reduce_double_action_not () :
crule<reduce_double_action_not>::crule("reduce_double_action_not") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_repeat_out_action_while : public crule<reduce_repeat_out_action_while> {
reduce_repeat_out_action_while () :
crule<reduce_repeat_out_action_while>::crule("reduce_repeat_out_action_while") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_repeat_in_action_while : public
crule<reduce_repeat_in_action_while> {
reduce_repeat_in_action_while() : crule<reduce_repeat_in_action_while>::crule("reduce_repeat_in_action_while") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_action_boolean_if_sub_cond : public
crule<reduce_action_boolean_if_sub_cond> {
reduce_action_boolean_if_sub_cond() : crule<reduce_action_boolean_if_sub_cond>::crule("reduce_action_boolean_if_sub_cond") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_boolean_while_sub_cond : public
crule<reduce_boolean_while_sub_cond> {
reduce_boolean_while_sub_cond() : crule<reduce_boolean_while_sub_cond>::crule("reduce_boolean_while_sub_cond") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_action_action_if_always_succeeds :
public crule<reduce_action_action_if_always_succeeds> {
reduce_action_action_if_always_succeeds() : crule<reduce_action_action_if_always_succeeds>::crule("reduce_action_action_if_always_succeeds") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_action_action_if_always_fails :
public crule<reduce_action_action_if_always_fails> {
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_action_while_always_fails :
public crule<reduce_action_while_always_fails> {
reduce_action_while_always_fails() : crule<reduce_action_while_always_fails>::crule("reduce_action_while_always_fails") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_boolean_while_depend_condition :
public crule<reduce_boolean_while_depend_condition> {
reduce_boolean_while_depend_condition() : crule<reduce_boolean_while_depend_condition>::crule("reduce_boolean_while_depend_condition") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_sequential_and_always_fails :
public crule<reduce_sequential_and_always_fails> {
reduce_sequential_and_always_fails() : crule<reduce_sequential_and_always_fails>::crule("reduce_sequential_and_always_fails") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_sequential_or_always_succeeds :
public crule<reduce_sequential_or_always_succeeds> {
reduce_sequential_or_always_succeeds() : crule<reduce_sequential_or_always_succeeds>::crule("reduce_sequential_or_always_succeeds") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_idempotent : public crule<reduce_idempotent> {
reduce_idempotent() : crule<reduce_idempotent>::crule("reduce_idempotent") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_opposite : public crule<reduce_opposite> {
reduce_opposite() : crule<reduce_opposite>::crule("reduce_opposite") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_additive : public crule<reduce_additive> {
reduce_additive() : crule<reduce_additive>::crule("reduce_additive") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_zero_neutral : public crule<reduce_zero_neutral> {
reduce_zero_neutral() : crule<reduce_zero_neutral>::crule("reduce_zero_neutral") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_modular_argument : public crule<reduce_modular_argument> {
reduce_modular_argument() : crule<reduce_modular_argument>::crule("reduce_modular_argument") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct preconditions_check : public crule<preconditions_check> {
preconditions_check() : crule<preconditions_check>::crule("preconditions_check") {}
void operator() (combo_tree& tr, combo_tree::iterator it) const;
};
bool reduce_free_post_action (builtin_action pre_a, builtin_action post_a, bool free_pre_action_before, combo_tree& tr, combo_tree::iterator it);
}
}
#endif