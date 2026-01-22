#ifndef _OPENCOG_IMPULSE_LINK_H
#define _OPENCOG_IMPULSE_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class ImpulseLink : public FunctionLink
{
protected:
void init();
public:
ImpulseLink(const HandleSeq&&, Type=IMPULSE_LINK);
ImpulseLink(const ImpulseLink&) = delete;
ImpulseLink& operator=(const ImpulseLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ImpulseLink)
#define createImpulseLink CREATE_DECL(ImpulseLink)
}
#endif