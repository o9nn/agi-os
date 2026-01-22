#ifndef _OPENCOG_INCOMING_OF_LINK_H
#define _OPENCOG_INCOMING_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class IncomingOfLink : public FunctionLink
{
public:
IncomingOfLink(const HandleSeq&&, Type = INCOMING_OF_LINK);
IncomingOfLink(const IncomingOfLink&) = delete;
IncomingOfLink& operator=(const IncomingOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(IncomingOfLink)
#define createIncomingOfLink CREATE_DECL(IncomingOfLink)
}
#endif