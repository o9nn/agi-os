#ifndef _OPENCOG_PLUS_LINK_H
#define _OPENCOG_PLUS_LINK_H
#include <opencog/atoms/reduct/ArithmeticLink.h>
namespace opencog
{
class PlusLink : public ArithmeticLink
{
protected:
static Handle zero;
virtual ValuePtr kons(AtomSpace*, bool,
const ValuePtr&, const ValuePtr&) const;
void init(void);
public:
PlusLink(const Handle& a, const Handle& b);
PlusLink(const HandleSeq&&, Type=PLUS_LINK);
PlusLink(const PlusLink&) = delete;
PlusLink& operator=(const PlusLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PlusLink)
#define createPlusLink CREATE_DECL(PlusLink)
}
#endif