#ifndef _OPENCOG_MINUS_LINK_H
#define _OPENCOG_MINUS_LINK_H
#include <opencog/atoms/reduct/PlusLink.h>
namespace opencog
{
class MinusLink : public PlusLink
{
protected:
void init(void);
ValuePtr kons(AtomSpace*, bool, const ValuePtr&, const ValuePtr&) const;
public:
MinusLink(const Handle& a, const Handle& b);
MinusLink(const HandleSeq&&, Type=MINUS_LINK);
MinusLink(const MinusLink&) = delete;
MinusLink& operator=(const MinusLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(MinusLink)
#define createMinusLink CREATE_DECL(MinusLink)
}
#endif