#ifndef _OPENCOG_RULE_H_
#define _OPENCOG_RULE_H_
#include <boost/operators.hpp>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/ScopeLink.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/pattern/BindLink.h>
#include <opencog/unify/Unify.h>
#include <opencog/util/empty_string.h>
namespace opencog {
class Rule;
typedef std::shared_ptr<Rule> RulePtr;
#define createRule std::make_shared<Rule>
struct rule_ptr_less
{
bool operator()(const RulePtr& l, const RulePtr& r) const;
};
class RuleSet : public std::vector<RulePtr>,
public boost::totally_ordered<RuleSet>
{
typedef std::vector<RulePtr> super;
public:
void expand_meta_rules(AtomSpace& as);
HandleSet aliases() const;
std::pair<iterator, bool> insert(RulePtr rule);
template<typename It>
void insert(It from, It to)
{
for (; from != to; ++from)
insert(*from);
}
bool operator==(const RuleSet& other) const;
bool operator<(const RuleSet& other) const;
iterator find(const RulePtr& rule);
const_iterator find(const RulePtr& rule) const;
TruthValueSeq get_tvs() const;
std::string to_string(const std::string& indent=empty_string) const;
std::string to_short_string(const std::string& indent=empty_string) const;
};
typedef std::map<Rule, Unify::TypedSubstitution> RuleTypedSubstitutionMap;
typedef RuleTypedSubstitutionMap::value_type RuleTypedSubstitutionPair;
class Rule : public boost::totally_ordered<Rule>
{
public:
Rule();
explicit Rule(const Handle& rule);
Rule(const Rule& rule);
Rule(const Handle& rule_alias, const Handle& rbs);
Rule(const Handle& rule_alias, const Handle& rule, const Handle& rbs);
void init(const Handle& rule_member);
void init(const Handle& rule_alias, const Handle& rbs);
void init(const Handle& rule_alias, const Handle& rule, const Handle& rbs);
bool verify_rule();
bool operator==(const Rule& r) const;
bool operator<(const Rule& r) const;
Rule& operator=(const Rule& r);
void set_rule(const Handle&);
void set_name(const std::string&);
void set_category(const std::string&);
std::string& get_name();
const std::string& get_name() const;
Handle get_rule() const;
Handle get_alias() const;
Handle get_definition() const;
Handle get_rbs() const;
void add(AtomSpace&);
Handle get_vardecl() const;
const Variables& get_variables() const;
Handle get_implicant() const;
Handle get_implicand() const;
bool is_valid() const;
bool is_meta() const;
bool has_cycle() const;
HandleSeq get_clauses() const;
HandleSeq get_premises() const;
Handle get_conclusion() const;
HandlePairSeq get_conclusions() const;
TruthValuePtr get_tv() const;
RuleTypedSubstitutionMap unify_source(const Handle& source,
const Handle& vardecl=Handle::UNDEFINED,
const AtomSpace* queried_as=nullptr) const;
RuleTypedSubstitutionMap unify_target(const Handle& target,
const Handle& vardecl=Handle::UNDEFINED,
const AtomSpace* queried_as=nullptr) const;
static RuleSet strip_typed_substitution(const RuleTypedSubstitutionMap& rules);
Handle apply(AtomSpace& as) const;
void set_exhausted();
void reset_exhausted();
bool is_exhausted() const;
std::string to_string(const std::string& indent=empty_string) const;
std::string to_short_string(const std::string& indent=empty_string) const;
mutable bool premises_as_clauses;
private:
BindLinkPtr _rule;
Handle _rule_alias;
std::string _name;
Handle _rbs;
TruthValuePtr _tv;
bool _exhausted;
mutable std::mutex _mutex;
Rule rand_alpha_converted() const;
HandleSeq get_conclusion_patterns() const;
Handle get_conclusion_pattern(const Handle& h) const;
Handle get_execution_output_first_argument(const Handle& h) const;
Rule substituted(const Unify::TypedSubstitution& ts,
const AtomSpace* queried_as=nullptr) const;
};
std::string oc_to_string(const Rule& rule,
const std::string& indent=empty_string);
std::string oc_to_string(const RuleSet& rules,
const std::string& indent=empty_string);
std::string oc_to_string(const RuleTypedSubstitutionPair& rule_ts_pair,
const std::string& indent=empty_string);
std::string oc_to_string(const RuleTypedSubstitutionMap& rule_ts_map,
const std::string& indent=empty_string);
}
#endif