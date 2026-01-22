#ifndef _OPENCOG_STATE_LINK_H
#define _OPENCOG_STATE_LINK_H
#include <opencog/atoms/core/UniqueLink.h>
namespace opencog
{
class StateLink : public UniqueLink
{
protected:
void init(void);
virtual void setAtomSpace(AtomSpace*);
virtual void install(void);
public:
StateLink(const HandleSeq&&, Type=STATE_LINK);
StateLink(const Handle& alias, const Handle& body);
StateLink(const StateLink&) = delete;
StateLink& operator=(const StateLink&) = delete;
Handle get_alias(void) const { return _outgoing.at(0); }
Handle get_state(void) const { return _outgoing.at(1); }
static Handle get_state(const Handle& alias, const AtomSpace*);
static Handle get_state(const Handle& alias)
{ return get_state(alias, alias->getAtomSpace()); }
static Handle get_link(const Handle& alias, const AtomSpace*);
static Handle get_link(const Handle& alias)
{ return get_link(alias, alias->getAtomSpace()); }
Handle get_link(const AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(StateLink)
#define createStateLink CREATE_DECL(StateLink)
}
#endif