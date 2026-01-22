#ifndef _OPENCOG_SLEEP_LINK_H
#define _OPENCOG_SLEEP_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class SleepLink : public FunctionLink
{
public:
SleepLink(const HandleSeq&&, Type=SLEEP_LINK);
SleepLink(const SleepLink &) = delete;
SleepLink& operator=(const SleepLink &) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SleepLink)
#define createSleepLink CREATE_DECL(SleepLink)
}
#endif