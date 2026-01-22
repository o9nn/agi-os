#ifndef _OPENCOG_CONTROLPOLICY_H_
#define _OPENCOG_CONTROLPOLICY_H_
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/ure/ActionSelection.h>
#include "BIT.h"
#include "../UREConfig.h"
#include "../Rule.h"
class ControlPolicyUTest;
namespace opencog
{
typedef std::map<Handle, TruthValuePtr> HandleTVMap;
typedef std::pair<RuleTypedSubstitutionPair, double> RuleSelection;
class ControlPolicy
{
friend class ::ControlPolicyUTest;
public:
ControlPolicy(const UREConfig& ure_config, const BIT& bit,
const Handle& target, AtomSpace* control_as=nullptr);
~ControlPolicy();
const std::string preproof_predicate_name = "URE:BC:preproof-of";
RuleSet rules;
RuleSelection select_rule(AndBIT& andbit, BITNode& bitleaf);
static HandleSet rule_aliases(const RuleTypedSubstitutionMap& rules);
private:
const UREConfig& _ure_config;
const BIT& _bit;
const Handle& _target;
HandleTVMap _default_tvs;
AtomSpace* _control_as;
AtomSpacePtr _query_as;
std::map<Handle, HandleSet> _expansion_control_rules;
RuleTypedSubstitutionMap get_valid_rules(const AndBIT& andbit,
const BITNode& bitleaf);
RuleSelection select_rule(const AndBIT& andbit,
const BITNode& bitleaf,
const RuleTypedSubstitutionMap& rules);
HandleTVMap expansion_success_tvs(const AndBIT& andbit,
const BITNode& bitleaf,
const RuleTypedSubstitutionMap& rules);
std::vector<double> rule_weights(const HandleTVMap& success_tvs,
const RuleTypedSubstitutionMap& rules);
std::vector<double> rule_weights(
const HandleCounter& alias_weights,
const RuleTypedSubstitutionMap& inf_rules) const;
HandleCounter default_alias_weights(const RuleTypedSubstitutionMap& rules) const;
HandleSet active_expansion_control_rules(const AndBIT& andbit,
const BITNode& bitleaf,
const Handle& inf_rule_alias);
bool is_control_rule_active(const AndBIT& andbit,
const BITNode& bitleaf,
const Handle& ctrl_rule) const;
bool match(const Handle& pattern, const Handle& term,
const Handle& vardecl=Handle::UNDEFINED) const;
Handle get_antecedent_preproof(const Handle& ctrl_rule) const;
bool is_antecedent_preproof(const Handle& h) const;
Handle get_expansion(const Handle& ctrl_rule) const;
bool is_expansion(const Handle& h) const;
HandleSet fetch_expansion_control_rules(const Handle& inf_rule);
HandleSet fetch_expansion_control_rules(const Handle& inf_rule, int n);
Handle mk_vardecl_vardecl(const Handle& vardecl_var);
Handle mk_list_of_args_vardecl(const Handle& args_var);
Handle mk_expand_exec(const Handle& input_andbit_var,
const Handle& input_leaf_var,
const Handle& inf_rule,
const Handle& output_andbit_var);
Handle mk_preproof_eval(const Handle& preproof_args_var);
Handle mk_expansion_control_rules_query(const Handle& inf_rule, int n);
HandleSeq mk_pattern_vars(int n);
Handle mk_pattern_var(int i);
double get_actual_mean(TruthValuePtr tv) const;
};
}
#endif