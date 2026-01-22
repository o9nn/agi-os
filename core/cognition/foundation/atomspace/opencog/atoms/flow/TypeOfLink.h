#ifndef _OPENCOG_TYPE_OF_LINK_H
#define _OPENCOG_TYPE_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class TypeOfLink : public FunctionLink
{
public:
TypeOfLink(const HandleSeq&&, Type = TYPE_OF_LINK);
TypeOfLink(const TypeOfLink&) = delete;
TypeOfLink& operator=(const TypeOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TypeOfLink)
#define createTypeOfLink CREATE_DECL(TypeOfLink)
}
#endif