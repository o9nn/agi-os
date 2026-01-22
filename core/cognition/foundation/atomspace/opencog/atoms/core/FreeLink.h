#ifndef _OPENCOG_FREE_LINK_H
#define _OPENCOG_FREE_LINK_H
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/FreeVariables.h>
namespace opencog
{
class FreeLink : public Link
{
protected:
FreeVariables _vars;
void init(void);
public:
FreeLink(const HandleSeq&&, Type=FREE_LINK);
FreeLink(const FreeLink&) = delete;
FreeLink& operator=(const FreeLink&) = delete;
virtual ~FreeLink() {}
const FreeVariables& get_vars() const
{ return _vars; }
bool is_closed(void) const { return 0 == _vars.varseq.size(); }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FreeLink)
#define createFreeLink CREATE_DECL(FreeLink)
}
#endif