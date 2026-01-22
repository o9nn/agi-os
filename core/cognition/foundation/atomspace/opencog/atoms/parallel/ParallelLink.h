#ifndef _OPENCOG_PARALLEL_LINK_H
#define _OPENCOG_PARALLEL_LINK_H
#include <opencog/atoms/core/UnorderedLink.h>
namespace opencog
{
class AtomSpace;
class ParallelLink : public UnorderedLink
{
public:
ParallelLink(const HandleSeq&&, Type=PARALLEL_LINK);
ParallelLink(const ParallelLink&) = delete;
ParallelLink& operator=(const ParallelLink&) = delete;
virtual bool is_evaluatable() const { return true; }
virtual bool bevaluate(AtomSpace*, bool);
void evaluate_scratch(AtomSpace*, bool, AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ParallelLink)
#define createParallelLink CREATE_DECL(ParallelLink)
}
#endif