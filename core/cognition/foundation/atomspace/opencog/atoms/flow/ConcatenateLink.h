#ifndef _OPENCOG_CONCATENATE_LINK_H
#define _OPENCOG_CONCATENATE_LINK_H
#include <opencog/atoms/flow/CollectionOfLink.h>
namespace opencog
{
class ConcatenateLink : public CollectionOfLink
{
protected:
virtual ValuePtr rewrap_h(AtomSpace*, const Handle&);
virtual ValuePtr rewrap_v(AtomSpace*, const ValuePtr&);
public:
ConcatenateLink(const HandleSeq&&, Type = CONCATENATE_LINK);
ConcatenateLink(const ConcatenateLink&) = delete;
ConcatenateLink& operator=(const ConcatenateLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ConcatenateLink)
#define createConcatenateLink CREATE_DECL(ConcatenateLink)
}
#endif