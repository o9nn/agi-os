#ifndef _OPENCOG_UNIQUE_LINK_H
#define _OPENCOG_UNIQUE_LINK_H
#include <opencog/atoms/core/FreeLink.h>
namespace opencog
{
class UniqueLink : public FreeLink
{
protected:
void init(void);
virtual void setAtomSpace(AtomSpace *);
static Handle get_unique_nt(const Handle&, Type, bool, const AtomSpace*);
static Handle get_unique(const Handle&, Type, bool, const AtomSpace*);
public:
UniqueLink(const HandleSeq&&, Type=UNIQUE_LINK);
UniqueLink(const Handle& alias, const Handle& body);
UniqueLink(const UniqueLink&) = delete;
UniqueLink& operator=(const UniqueLink&) = delete;
Handle get_alias(void) const { return _outgoing.at(0); }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(UniqueLink)
#define createUniqueLink CREATE_DECL(UniqueLink)
}
#endif