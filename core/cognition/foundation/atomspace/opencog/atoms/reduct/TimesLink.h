#ifndef _OPENCOG_TIMES_LINK_H
#define _OPENCOG_TIMES_LINK_H
#include <opencog/atoms/reduct/ArithmeticLink.h>
namespace opencog
{
class TimesLink : public ArithmeticLink
{
protected:
static Handle one;
ValuePtr kons(AtomSpace*, bool,
const ValuePtr&, const ValuePtr&) const;
void init(void);
public:
TimesLink(const HandleSeq&&, Type=TIMES_LINK);
TimesLink(const Handle& a, const Handle& b);
TimesLink(const TimesLink&) = delete;
TimesLink& operator=(const TimesLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TimesLink)
#define createTimesLink CREATE_DECL(TimesLink)
}
#endif