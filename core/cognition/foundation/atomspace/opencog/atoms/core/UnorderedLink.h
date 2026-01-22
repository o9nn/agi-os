#ifndef _OPENCOG_UNORDERED_LINK_H
#define _OPENCOG_UNORDERED_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class UnorderedLink : public Link
{
public:
UnorderedLink(const HandleSeq&&, Type=UNORDERED_LINK);
UnorderedLink(const HandleSet&, Type=UNORDERED_LINK);
UnorderedLink(const UnorderedLink&) = delete;
UnorderedLink& operator=(const UnorderedLink&) = delete;
virtual bool is_unordered_link() const { return true; }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(UnorderedLink)
#define createUnorderedLink CREATE_DECL(UnorderedLink)
}
#endif