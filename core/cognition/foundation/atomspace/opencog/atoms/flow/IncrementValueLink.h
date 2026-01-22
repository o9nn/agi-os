#ifndef _OPENCOG_INCREMENT_VALUE_LINK_H
#define _OPENCOG_INCREMENT_VALUE_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class IncrementValueLink : public FunctionLink
{
public:
IncrementValueLink(const HandleSeq&&, Type=INCREMENT_VALUE_LINK);
IncrementValueLink(const IncrementValueLink&) = delete;
IncrementValueLink& operator=(const IncrementValueLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(IncrementValueLink)
#define createIncrementValueLink CREATE_DECL(IncrementValueLink)
}
#endif