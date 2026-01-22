#ifndef _OPENCOG_ACCUMULATE_LINK_H
#define _OPENCOG_ACCUMULATE_LINK_H
#include <opencog/atoms/reduct/NumericFunctionLink.h>
namespace opencog
{
class AccumulateLink : public NumericFunctionLink
{
protected:
void init(void);
public:
AccumulateLink(const Handle&);
AccumulateLink(const HandleSeq&&, Type=ACCUMULATE_LINK);
AccumulateLink(const AccumulateLink&) = delete;
AccumulateLink& operator=(const AccumulateLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(AccumulateLink)
#define createAccumulateLink CREATE_DECL(AccumulateLink)
}
#endif