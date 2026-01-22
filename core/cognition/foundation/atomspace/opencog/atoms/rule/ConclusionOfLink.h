#ifndef _OPENCOG_CONCLUSION_OF_LINK_H
#define _OPENCOG_CONCLUSION_OF_LINK_H
#include <opencog/atoms/rule/VardeclOfLink.h>
#include <opencog/atoms/rule/RuleLink.h>
namespace opencog
{
class ConclusionOfLink : public VardeclOfLink
{
private:
void init(void);
protected:
RuleLinkPtr _rule;
Handle _conclusion;
public:
ConclusionOfLink(const HandleSeq&&, Type=CONCLUSION_OF_LINK);
ConclusionOfLink(const ConclusionOfLink&) = delete;
ConclusionOfLink& operator=(const ConclusionOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ConclusionOfLink)
#define createConclusionOfLink CREATE_DECL(ConclusionOfLink)
}
#endif