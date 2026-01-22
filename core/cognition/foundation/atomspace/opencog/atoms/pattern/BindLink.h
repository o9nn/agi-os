#ifndef _OPENCOG_BIND_LINK_H
#define _OPENCOG_BIND_LINK_H
#include <opencog/atoms/pattern/QueryLink.h>
namespace opencog
{
class BindLink : public QueryLink
{
protected:
void init(void);
public:
BindLink(const HandleSeq&&, Type=BIND_LINK);
BindLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
BindLink(const Handle& body, const Handle& rewrite);
BindLink(const BindLink&) = delete;
BindLink& operator=(const BindLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(BindLink)
#define createBindLink CREATE_DECL(BindLink)
}
#endif