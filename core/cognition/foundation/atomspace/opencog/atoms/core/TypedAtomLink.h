#ifndef _OPENCOG_TYPED_ATOM_LINK_H
#define _OPENCOG_TYPED_ATOM_LINK_H
#include <opencog/atoms/core/UniqueLink.h>
namespace opencog
{
class TypedAtomLink : public UniqueLink
{
protected:
void init();
public:
TypedAtomLink(const HandleSeq&&, Type=TYPED_ATOM_LINK);
TypedAtomLink(const Handle& alias, const Handle& body);
TypedAtomLink(const TypedAtomLink&) = delete;
TypedAtomLink& operator=(const TypedAtomLink&) = delete;
Handle get_atom(void) const { return _outgoing.at(0); }
Handle get_type(void) const { return _outgoing.at(1); }
static Handle get_type(const Handle&, const AtomSpace*);
static Handle get_link(const Handle&, const AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TypedAtomLink)
#define createTypedAtomLink CREATE_DECL(TypedAtomLink)
}
#endif