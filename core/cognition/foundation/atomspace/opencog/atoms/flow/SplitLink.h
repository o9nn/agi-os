#ifndef _OPENCOG_SPLIT_LINK_H
#define _OPENCOG_SPLIT_LINK_H
#include <opencog/atoms/flow/CollectionOfLink.h>
namespace opencog
{
class SplitLink : public CollectionOfLink
{
protected:
std::string _sep;
virtual ValuePtr rewrap_h(AtomSpace*, const Handle&);
virtual ValuePtr rewrap_v(AtomSpace*, const ValuePtr&);
public:
SplitLink(const HandleSeq&&, Type = SPLIT_LINK);
SplitLink(const SplitLink&) = delete;
SplitLink& operator=(const SplitLink&) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SplitLink)
#define createSplitLink CREATE_DECL(SplitLink)
}
#endif