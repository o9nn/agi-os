#ifndef _OPENCOG_SATISFACTION_LINK_H
#define _OPENCOG_SATISFACTION_LINK_H
#include <opencog/atoms/pattern/PatternLink.h>
namespace opencog
{
class SatisfactionLink : public PatternLink
{
protected:
void init(void);
public:
SatisfactionLink(const HandleSeq&&, Type=SATISFACTION_LINK);
SatisfactionLink(const SatisfactionLink&) = delete;
SatisfactionLink& operator=(const SatisfactionLink&) = delete;
virtual bool is_evaluatable() const { return true; }
virtual bool bevaluate(AtomSpace*, bool);
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace* as, bool silent) {
return ValueCast(evaluate(as, silent)); }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SatisfactionLink)
#define createSatisfactionLink CREATE_DECL(SatisfactionLink)
}
#endif