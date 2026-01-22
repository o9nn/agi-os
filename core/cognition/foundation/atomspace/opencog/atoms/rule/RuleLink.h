#ifndef _OPENCOG_RULE_LINK_H
#define _OPENCOG_RULE_LINK_H
#include <opencog/atoms/core/PrenexLink.h>
namespace opencog
{
class RuleLink : public PrenexLink
{
protected:
void init(void);
HandleSeq _implicand;
void extract_variables(const HandleSeq& oset);
public:
RuleLink(const HandleSeq&&, Type=RULE_LINK);
RuleLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
RuleLink(const Handle& body, const Handle& rewrite);
RuleLink(const RuleLink&) = delete;
RuleLink& operator=(const RuleLink&) = delete;
virtual const HandleSeq& get_implicand(void) { return _implicand; }
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(RuleLink)
#define createRuleLink CREATE_DECL(RuleLink)
}
#endif