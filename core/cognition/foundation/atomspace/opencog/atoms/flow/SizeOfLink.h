#ifndef _OPENCOG_SIZE_OF_LINK_H
#define _OPENCOG_SIZE_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class SizeOfLink : public FunctionLink
{
public:
SizeOfLink(const HandleSeq&&, Type = SIZE_OF_LINK);
SizeOfLink(const SizeOfLink&) = delete;
SizeOfLink& operator=(const SizeOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SizeOfLink)
#define createSizeOfLink CREATE_DECL(SizeOfLink)
}
#endif