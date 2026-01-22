#ifndef _OPENCOG_VARDECL_OF_LINK_H
#define _OPENCOG_VARDECL_OF_LINK_H
#include <opencog/atoms/core/PrenexLink.h>
namespace opencog
{
class VardeclOfLink : public Link
{
private:
void init(void);
protected:
PrenexLinkPtr _lambda;
Handle _vardecl;
const Handle& term_at(const HandleSeq&);
public:
VardeclOfLink(const HandleSeq&&, Type=VARDECL_OF_LINK);
VardeclOfLink(const VardeclOfLink&) = delete;
VardeclOfLink& operator=(const VardeclOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(VardeclOfLink)
#define createVardeclOfLink CREATE_DECL(VardeclOfLink)
}
#endif