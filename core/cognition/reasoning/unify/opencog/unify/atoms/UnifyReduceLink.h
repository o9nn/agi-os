#ifndef _OPENCOG_UNIFY_REDUCE_LINK_H
#define _OPENCOG_UNIFY_REDUCE_LINK_H
#include <opencog/unify/atoms/UnifierLink.h>
namespace opencog
{
class UnifyReduceLink : public UnifierLink
{
public:
UnifyReduceLink(const HandleSeq&&, Type = UNIFY_REDUCE_LINK);
UnifyReduceLink(const UnifyReduceLink&) = delete;
UnifyReduceLink& operator=(const UnifyReduceLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(UnifyReduceLink)
#define createUnifyReduceLink CREATE_DECL(UnifyReduceLink)
}
#endif