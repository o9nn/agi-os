#ifndef _OPENCOG_DEFINE_LINK_H
#define _OPENCOG_DEFINE_LINK_H
#include <opencog/atoms/core/UniqueLink.h>
namespace opencog
{
class DefineLink : public UniqueLink
{
protected:
void init(void);
public:
DefineLink(const HandleSeq&&, Type=DEFINE_LINK);
DefineLink(const Handle& alias, const Handle& body);
DefineLink(const DefineLink&) = delete;
DefineLink& operator=(const DefineLink&) = delete;
Handle get_alias(void) const { return _outgoing.at(0); }
Handle get_definition(void) const { return _outgoing.at(1); }
static Handle get_definition(const Handle& alias, const AtomSpace*);
static Handle get_definition(const Handle& alias)
{ return get_definition(alias, alias->getAtomSpace()); }
static Handle get_link(const Handle& alias, const AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DefineLink)
#define createDefineLink CREATE_DECL(DefineLink)
}
#endif