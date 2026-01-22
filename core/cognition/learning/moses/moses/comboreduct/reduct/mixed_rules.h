#ifndef _REDUCT_MIXED_RULES_H
#define _REDUCT_MIXED_RULES_H
#include "reduct.h"
namespace opencog { namespace reduct {
struct reduce_gt_zero_times_const : public crule<reduce_gt_zero_times_const> {
reduce_gt_zero_times_const() : crule<reduce_gt_zero_times_const>::crule("reduce_gt_zero_times_const") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_gt_zero_pair_power : public crule<reduce_gt_zero_pair_power> {
reduce_gt_zero_pair_power() : crule<reduce_gt_zero_pair_power>::crule("reduce_gt_zero_pair_power") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_gt_zero_const_div : public crule<reduce_gt_zero_const_div>
{
reduce_gt_zero_const_div() : crule<reduce_gt_zero_const_div>::crule("reduce_gt_zero_const_div") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
#ifndef ABS_LOG
struct reduce_gt_zero_log : public crule<reduce_gt_zero_log>
{
reduce_gt_zero_log() : crule<reduce_gt_zero_log>::crule("reduce_gt_zero_log") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
#endif
struct reduce_gt_zero_exp : public crule<reduce_gt_zero_exp>
{
reduce_gt_zero_exp() : crule<reduce_gt_zero_exp>::crule("reduce_gt_zero_exp") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_gt_zero_minus_exp : public crule<reduce_gt_zero_minus_exp>
{
reduce_gt_zero_minus_exp() : crule<reduce_gt_zero_minus_exp>::crule("reduce_gt_zero_minus_exp") {}
void operator()(combo_tree& tr, combo_tree::iterator it) const;
};
struct reduce_gt_zero_prod_exp : public crule<reduce_gt_zero_prod_exp> {
reduce_gt_zero_prod_exp() : crule<reduce_gt_zero_prod_exp>::crule("reduce_gt_zero_prod_exp") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_gt_zero_const_sum_sin : public crule<reduce_gt_zero_const_sum_sin> {
reduce_gt_zero_const_sum_sin() : crule<reduce_gt_zero_const_sum_sin>::crule("reduce_gt_zero_const_sum_sin") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_gt_zero_impulse : public crule<reduce_gt_zero_impulse> {
reduce_gt_zero_impulse() : crule<reduce_gt_zero_impulse>::crule("reduce_gt_zero_impulse") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_impulse_power : public crule<reduce_impulse_power> {
reduce_impulse_power() : crule<reduce_impulse_power>::crule("reduce_impulse_power") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_impulse_prod : public crule<reduce_impulse_prod> {
reduce_impulse_prod() : crule<reduce_impulse_prod>::crule("reduce_impulse_prod") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_impulse_sum : public crule<reduce_impulse_sum> {
reduce_impulse_sum() : crule<reduce_impulse_sum>::crule("reduce_impulse_sum") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_contin_if_to_impulse : public crule<reduce_contin_if_to_impulse> {
reduce_contin_if_to_impulse() : crule<reduce_contin_if_to_impulse>::crule("reduce_contin_if_to_impulse") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_contin_if : public crule<reduce_contin_if> {
reduce_contin_if() : crule<reduce_contin_if>::crule("reduce_contin_if") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_op_contin_if : public crule<reduce_op_contin_if> {
reduce_op_contin_if() : crule<reduce_op_contin_if>::crule("reduce_op_contin_if") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_contin_if_inner_op : public crule<reduce_contin_if_inner_op> {
reduce_contin_if_inner_op() : crule<reduce_contin_if_inner_op>::crule("reduce_contin_if_inner_op") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_contin_if_substitute_cond
: public crule<reduce_contin_if_substitute_cond>
{
reduce_contin_if_substitute_cond() : crule<reduce_contin_if_substitute_cond>::crule("reduce_contin_if_substitute_cond") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_junction_gt_zero_sum_constant
: public crule<reduce_junction_gt_zero_sum_constant>
{
reduce_junction_gt_zero_sum_constant() : crule<reduce_junction_gt_zero_sum_constant>::crule("reduce_junction_gt_zero_sum_constant") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_from_assumptions : public crule<reduce_from_assumptions> {
reduce_from_assumptions(const rule& r) : crule<reduce_from_assumptions>::crule("reduce_from_assumptions"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
bool implies(const combo_tree& tr, combo_tree::iterator it1, combo_tree::iterator it2) const;
protected:
const rule* _reduction;
};
struct reduce_contin_if_not : public crule<reduce_contin_if_not> {
reduce_contin_if_not(const rule& r) : crule<reduce_contin_if_not>::crule("reduce_contin_if_not"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_gt_zero_sum : public crule<reduce_gt_zero_sum> {
reduce_gt_zero_sum(const rule& r) : crule<reduce_gt_zero_sum>::crule("reduce_gt_zero_sum"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_gt_zero_prod : public crule<reduce_gt_zero_prod> {
reduce_gt_zero_prod(const rule& r)
: crule<reduce_gt_zero_prod>::crule("reduce_gt_zero_prod"),
_complete_reduction(&r), _reduction_without_itself(&r) { }
reduce_gt_zero_prod(const rule& r1, const rule& r2)
: crule<reduce_gt_zero_prod>::crule("reduce_gt_zero_prod"),
_complete_reduction(&r1), _reduction_without_itself(&r2) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _complete_reduction;
const rule* _reduction_without_itself;
};
struct reduce_gt_zero_div : public crule<reduce_gt_zero_div> {
reduce_gt_zero_div(const rule& r) : crule<reduce_gt_zero_div>::crule("reduce_gt_zero_div"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_gt_zero_sum_sin : public crule<reduce_gt_zero_sum_sin> {
reduce_gt_zero_sum_sin(const rule& r) : crule<reduce_gt_zero_sum_sin>::crule("reduce_gt_zero_sum_sin"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_gt_zero_sin : public crule<reduce_gt_zero_sin> {
reduce_gt_zero_sin(const rule& r) : crule<reduce_gt_zero_sin>::crule("reduce_gt_zero_sin"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_gt_division_of_constants : public crule<reduce_gt_division_of_constants> {
reduce_gt_division_of_constants(const rule& r) : crule<reduce_gt_division_of_constants>::crule("reduce_gt_division_of_constants"), _reduction(&r) { }
void operator()(combo_tree& tr,combo_tree::iterator it) const;
protected:
const rule* _reduction;
};
struct reduce_inequality_from_assumptions : public crule<reduce_inequality_from_assumptions> {
reduce_inequality_from_assumptions() :
crule<reduce_inequality_from_assumptions>::crule("reduce_inequality_from_assumptions") {}
typedef std::vector< std::vector<double> > double_matrix;
void operator()(combo_tree& tr,combo_tree::iterator it) const;
contin_t splitCoefExpression(combo_tree& tr, combo_tree::iterator& it) const;
bool gaussJordanElimination(double_matrix& dm, double eps = 1.0e-10) const;
void gaussianElimination(double_matrix& dm) const;
bool gaussianElimination2(double_matrix& dm, double eps = 1.0e-10) const;
bool backSubstitution(double_matrix& dm, std::vector<double>& solution, double eps = 1.0e-10) const;
};
}
}
#endif