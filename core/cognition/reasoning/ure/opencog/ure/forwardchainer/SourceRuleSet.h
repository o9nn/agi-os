#ifndef _OPENCOG_SOURCERULESET_H_
#define _OPENCOG_SOURCERULESET_H_
#include <opencog/util/empty_string.h>
#include "../ThompsonSampling.h"
#include "SourceSet.h"
namespace opencog
{
class SourceRule : public boost::totally_ordered<SourceRule>
{
public:
SourceRule(SourcePtr src=nullptr, RulePtr rule=nullptr);
~SourceRule();
bool operator==(const SourceRule& other) const;
bool operator<(const SourceRule& other) const;
bool is_valid() const;
std::string to_string(const std::string& indent=empty_string) const;
SourcePtr source;
RulePtr rule;
};
class SourceRuleSet
{
public:
SourceRuleSet();
bool insert(const SourceRule& sr, TruthValuePtr tv);
std::pair<SourceRule, TruthValuePtr> thompson_select();
bool empty() const;
size_t size() const;
std::string to_string(const std::string& indent=empty_string) const;
std::vector<SourceRule> source_rule_seq;
TruthValueSeq tv_seq;
private:
ThompsonSampling _thompson_smp;
};
std::string oc_to_string(const SourceRule& sr,
const std::string& indent=empty_string);
std::string oc_to_string(const SourceRuleSet& srs,
const std::string& indent=empty_string);
}
#endif