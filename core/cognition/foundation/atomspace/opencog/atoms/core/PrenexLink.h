#ifndef _OPENCOG_PRENEX_LINK_H
#define _OPENCOG_PRENEX_LINK_H
#include <opencog/atoms/core/RewriteLink.h>
namespace opencog
{
class PrenexLink : public RewriteLink
{
protected:
void init(void);
Handle reassemble(Type, const HandleMap&, const Variables&) const;
public:
PrenexLink(const HandleSeq&&, Type=PRENEX_LINK);
PrenexLink(const Handle& varcdecls, const Handle& body);
PrenexLink(const PrenexLink &) = delete;
PrenexLink& operator=(const PrenexLink &) = delete;
virtual Handle beta_reduce(const HandleSeq& seq) const;
virtual Handle beta_reduce(const HandleMap& vm) const;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PrenexLink)
#define createPrenexLink CREATE_DECL(PrenexLink)
}
#endif