#ifndef _OPENCOG_MIN_LINK_H
#define _OPENCOG_MIN_LINK_H
#include <opencog/atoms/reduct/NumericFunctionLink.h>
namespace opencog
{
class MinLink : public NumericFunctionLink
{
protected:
void init(void);
public:
MinLink(const HandleSeq&&, Type=MIN_LINK);
MinLink(const MinLink&) = delete;
MinLink& operator=(const MinLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(MinLink)
#define createMinLink CREATE_DECL(MinLink)
}
#endif