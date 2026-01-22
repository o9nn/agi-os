#ifndef _OPENCOG_COLLECTION_OF_LINK_H
#define _OPENCOG_COLLECTION_OF_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class CollectionOfLink : public FunctionLink
{
protected:
Type _out_type;
bool _out_is_link;
bool _have_typespec;
void check_typespec(void);
virtual ValuePtr rewrap_h(AtomSpace*, const Handle&);
virtual ValuePtr rewrap_v(AtomSpace*, const ValuePtr&);
public:
CollectionOfLink(const HandleSeq&&, Type = COLLECTION_OF_LINK);
CollectionOfLink(const CollectionOfLink&) = delete;
CollectionOfLink& operator=(const CollectionOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(CollectionOfLink)
#define createCollectionOfLink CREATE_DECL(CollectionOfLink)
}
#endif