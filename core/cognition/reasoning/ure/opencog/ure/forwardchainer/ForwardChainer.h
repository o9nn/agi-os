#ifndef _OPENCOG_FORWARDCHAINER_H_
#define _OPENCOG_FORWARDCHAINER_H_
#include <mutex>
#include "../UREConfig.h"
#include "SourceSet.h"
#include "SourceRuleSet.h"
#include "FCStat.h"
class ForwardChainerUTest;
namespace opencog
{
enum class source_selection_mode
{
TV_FITNESS, STI, UNIFORM
};
class Rule;
typedef std::pair<RulePtr, double> RuleProbabilityPair;
class ForwardChainer
{
public:
ForwardChainer(AtomSpace& kb_as,
AtomSpace& rb_as,
const Handle& rbs,
const Handle& source,
const Handle& vardecl=Handle::UNDEFINED,
AtomSpace* trace_as=nullptr,
const HandleSeq& focus_set=HandleSeq());
ForwardChainer(AtomSpace& kb_as,
const Handle& rbs,
const Handle& source,
const Handle& vardecl=Handle::UNDEFINED,
AtomSpace* trace_as=nullptr,
const HandleSeq& focus_set=HandleSeq());
~ForwardChainer();
UREConfig& get_config();
const UREConfig& get_config() const;
void do_chain();
void do_steps_singlethread();
void do_steps_multithread();
void do_steps_srpi();
void do_step(int iteration);
void do_step_srpi(int iteration);
bool termination();
void termination_log();
Handle get_results() const;
HandleSet get_results_set() const;
private:
friend class ::ForwardChainerUTest;
void init(const Handle& source,
const Handle& vardecl,
const HandleSeq& focus_set);
void apply_all_rules();
void validate(const Handle& source);
void expand_meta_rules(const std::string& msgprfx);
SourcePtr select_source(const std::string& msgprfx);
SourceRule mk_source_rule(const std::string& msgprfx);
void populate_source_rule_set(const std::string& msgprfx);
std::pair<SourceRule, TruthValuePtr>
select_source_rule(const std::string& msgprfx);
TruthValuePtr calculate_source_rule_tv(const SourceRule& sr);
RuleSet get_valid_rules(const Source& source);
RuleProbabilityPair select_rule(const Handle& source,
const std::string& msgprfx="");
RuleProbabilityPair select_rule(Source& source,
const std::string& msgprfx="");
RuleProbabilityPair select_rule(const RuleSet&,
const std::string& msgprfx="");
HandleSet apply_rule(const Rule& rule);
HandleSet apply_rule(const SourceRule& sr);
RuleSet _rules;
AtomSpace& _kb_as;
AtomSpace& _rb_as;
AtomSpacePtr _focus_set_as;
UREConfig _config;
std::atomic<int> _iteration;
bool _search_focus_set;
mutable std::mutex _whole_mutex;
mutable std::mutex _part_mutex;
mutable std::mutex _rules_mutex;
std::atomic<int> _thread_count;
SourceSet _sources;
FCStat _fcstat;
const bool _srpi;
SourceRuleSet _source_rule_set;
};
}
#endif