#ifndef _OPENCOG_DELETE_LINK_H
#define _OPENCOG_DELETE_LINK_H
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/FreeLink.h>
namespace opencog
{
class DeleteLink : public FreeLink
{
protected:
void init(void);
void setAtomSpace(AtomSpace *);
public:
DeleteLink(const HandleSeq&&, Type=DELETE_LINK);
DeleteLink(const DeleteLink&) = delete;
DeleteLink& operator=(const DeleteLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DeleteLink)
#define createDeleteLink CREATE_DECL(DeleteLink)
}
#endif