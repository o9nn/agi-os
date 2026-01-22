#ifndef _OPENCOG_LINK_COLUMN_H
#define _OPENCOG_LINK_COLUMN_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class LinkColumn : public Link
{
protected:
ValuePtr do_execute(AtomSpace*, bool);
ValuePtr do_handle_loop(AtomSpace*, bool, const HandleSeq&);
public:
LinkColumn(const HandleSeq&&, Type = LINK_COLUMN);
LinkColumn(const LinkColumn&) = delete;
LinkColumn& operator=(const LinkColumn&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(LinkColumn)
#define createLinkColumn CREATE_DECL(LinkColumn)
}
#endif