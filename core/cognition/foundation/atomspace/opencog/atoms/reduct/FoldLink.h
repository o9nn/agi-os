#ifndef _OPENCOG_FOLD_LINK_H
#define _OPENCOG_FOLD_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class FoldLink : public FunctionLink
{
protected:
ValuePtr knil;
virtual ValuePtr kons(AtomSpace*, bool,
const ValuePtr&, const ValuePtr&) const = 0;
void init(void);
public:
FoldLink(const HandleSeq&&, Type);
FoldLink(const FoldLink&) = delete;
FoldLink& operator=(const FoldLink&) = delete;
virtual ValuePtr delta_reduce(AtomSpace*, bool) const;
};
LINK_PTR_DECL(FoldLink)
#define createFoldLink CREATE_DECL(FoldLink)
}
#endif