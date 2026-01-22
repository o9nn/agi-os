#ifndef _OPENCOG_GRANT_LINK_H
#define _OPENCOG_GRANT_LINK_H
#include <opencog/atoms/core/UniqueLink.h>
namespace opencog
{
class GrantLink : public UniqueLink
{
protected:
void init(void);
virtual ContentHash compute_hash() const;
virtual void setAtomSpace(AtomSpace*);
public:
GrantLink(const HandleSeq&&, Type=GRANT_LINK);
GrantLink(const Handle& alias, const Handle& body);
GrantLink(const GrantLink&) = delete;
GrantLink& operator=(const GrantLink&) = delete;
virtual bool operator==(const Atom&) const;
Handle get_alias(void) const { return _outgoing.at(0); }
static Handle get_link(const Handle& alias, const AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(GrantLink)
#define createGrantLink CREATE_DECL(GrantLink)
}
#endif