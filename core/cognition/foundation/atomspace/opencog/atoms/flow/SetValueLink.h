#ifndef _OPENCOG_SET_VALUE_LINK_H
#define _OPENCOG_SET_VALUE_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class SetValueLink : public FunctionLink
{
public:
SetValueLink(const HandleSeq&&, Type=SET_VALUE_LINK);
SetValueLink(const SetValueLink&) = delete;
SetValueLink& operator=(const SetValueLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SetValueLink)
#define createSetValueLink CREATE_DECL(SetValueLink)
}
#endif