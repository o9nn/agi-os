#ifndef _REDUCT_CONTIN_RULES_H
#define _REDUCT_CONTIN_RULES_H
#include "reduct.h"
namespace opencog { namespace reduct {
struct reduce_plus_zero : public crule<reduce_plus_zero> {
reduce_plus_zero() : crule<reduce_plus_zero>::crule("reduce_plus_zero") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_times_one_zero : public crule<reduce_times_one_zero> {
reduce_times_one_zero() : crule<reduce_times_one_zero>::crule("reduce_times_one_zero") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_factorize_fraction : public crule<reduce_factorize_fraction> {
reduce_factorize_fraction() : crule<reduce_factorize_fraction>::crule("reduce_factorize_fraction") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_factorize : public crule<reduce_factorize> {
reduce_factorize() : crule<reduce_factorize>::crule("reduce_factorize") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_distribute : public crule<reduce_distribute> {
reduce_distribute(const rule& r) :
crule<reduce_distribute>::crule("reduce_distribute"), _reduction(&r) {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_invert_constant : public crule<reduce_invert_constant> {
reduce_invert_constant() : crule<reduce_invert_constant>::crule("reduce_invert_constant") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_fraction : public crule<reduce_fraction> {
reduce_fraction() : crule<reduce_fraction>::crule("reduce_fraction") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_times_div : public crule<reduce_times_div> {
reduce_times_div() : crule<reduce_times_div>::crule("reduce_times_div") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_plus_times_one_child : public crule<reduce_plus_times_one_child> {
reduce_plus_times_one_child() : crule<reduce_plus_times_one_child>::crule("reduce_plus_times_one_child") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_sum_log : public crule<reduce_sum_log> {
reduce_sum_log() : crule<reduce_sum_log>::crule("reduce_sum_log") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_log_div_times : public crule<reduce_log_div_times> {
reduce_log_div_times() : crule<reduce_log_div_times>::crule("reduce_log_div_times") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_exp_times : public crule<reduce_exp_times> {
reduce_exp_times() : crule<reduce_exp_times>::crule("reduce_exp_times") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_exp_div : public crule<reduce_exp_div> {
reduce_exp_div() : crule<reduce_exp_div>::crule("reduce_exp_div") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
#ifndef ABS_LOG
struct reduce_exp_log : public crule<reduce_exp_log> {
reduce_exp_log() : crule<reduce_exp_log>::crule("reduce_exp_log") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
#endif
struct reduce_sin : public crule<reduce_sin> {
reduce_sin() : crule<reduce_sin>::crule("reduce_sin") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_impulse_arg : public crule<reduce_impulse_arg>
{
int reduct_effort;
const vertex_set &ignore_ops;
reduce_impulse_arg(int effort, const vertex_set &igop)
: crule<reduce_impulse_arg>::crule("reduce_impulse_arg"),
reduct_effort(effort), ignore_ops(igop) {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
}
}
#endif