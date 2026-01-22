#ifndef _OPENCOG_DIVIDE_LINK_H
#define _OPENCOG_DIVIDE_LINK_H
#include <opencog/atoms/reduct/TimesLink.h>
namespace opencog
{
class DivideLink : public TimesLink
{
protected:
void init(void);
ValuePtr kons(AtomSpace*, bool, const ValuePtr&, const ValuePtr&) const;
public:
DivideLink(const Handle& a, const Handle& b);
DivideLink(const HandleSeq&&, Type=DIVIDE_LINK);
DivideLink(const DivideLink&) = delete;
DivideLink& operator=(const DivideLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DivideLink)
#define createDivideLink CREATE_DECL(DivideLink)
}
#endif