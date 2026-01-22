#ifndef _OPENCOG_VALUE_OF_LINK_H
#define _OPENCOG_VALUE_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class ValueOfLink : public FunctionLink
{
private:
void init(void);
protected:
ValuePtr do_execute(AtomSpace*, bool);
public:
ValueOfLink(const HandleSeq&&, Type=VALUE_OF_LINK);
ValueOfLink(const ValueOfLink&) = delete;
ValueOfLink& operator=(const ValueOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ValueOfLink)
#define createValueOfLink CREATE_DECL(ValueOfLink)
}
#endif