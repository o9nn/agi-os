#ifndef _OPENCOG_TIME_LINK_H
#define _OPENCOG_TIME_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class TimeLink : public FunctionLink
{
public:
TimeLink(const HandleSeq&&, Type=TIME_LINK);
TimeLink(const TimeLink&) = delete;
TimeLink& operator=(const TimeLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TimeLink)
#define createTimeLink CREATE_DECL(TimeLink)
}
#endif