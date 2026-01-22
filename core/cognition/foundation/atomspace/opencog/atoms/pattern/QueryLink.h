#ifndef _OPENCOG_QUERY_LINK_H
#define _OPENCOG_QUERY_LINK_H
#include <opencog/atoms/pattern/PatternLink.h>
#include <opencog/atoms/value/ContainerValue.h>
namespace opencog
{
class QueryLink : public PatternLink
{
protected:
void init(void);
virtual ContainerValuePtr do_execute(AtomSpace*, bool silent);
public:
QueryLink(const HandleSeq&&, Type=QUERY_LINK);
QueryLink(const Handle& vardecl, const Handle& body, const Handle& rewrite);
QueryLink(const Handle& body, const Handle& rewrite);
QueryLink(const QueryLink&) = delete;
QueryLink& operator=(const QueryLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool silent=false);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(QueryLink)
#define createQueryLink CREATE_DECL(QueryLink)
}
#endif