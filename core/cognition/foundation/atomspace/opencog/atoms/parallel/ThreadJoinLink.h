#ifndef _OPENCOG_THREAD_JOIN_LINK_H
#define _OPENCOG_THREAD_JOIN_LINK_H
#include <opencog/atoms/parallel/ParallelLink.h>
namespace opencog
{
class AtomSpace;
class ThreadJoinLink : public ParallelLink
{
public:
ThreadJoinLink(const HandleSeq&&, Type=THREAD_JOIN_LINK);
ThreadJoinLink(const ThreadJoinLink&) = delete;
ThreadJoinLink& operator=(const ThreadJoinLink&) = delete;
virtual bool bevaluate(AtomSpace*, bool);
bool evaluate_scratch(AtomSpace*, bool, AtomSpace*);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ThreadJoinLink)
#define createThreadJoinLink CREATE_DECL(ThreadJoinLink)
}
#endif