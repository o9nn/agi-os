#ifndef _OPENCOG_CONTINUATION_MIXIN_H
#define _OPENCOG_CONTINUATION_MIXIN_H
#include <opencog/query/InitiateSearchMixin.h>
#include <opencog/query/SatisfyMixin.h>
#include <opencog/query/TermMatchMixin.h>
namespace opencog {
class AtomSpace;
class ContinuationMixin :
public TermMatchMixin,
public InitiateSearchMixin,
public SatisfyMixin
{
public:
ContinuationMixin(AtomSpace* as) :
TermMatchMixin(as), InitiateSearchMixin(as),
_continuation(nullptr)
{}
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
TermMatchMixin::set_pattern(vars, pat);
InitiateSearchMixin::set_pattern(vars, pat);
}
virtual bool evaluate_sentence(const Handle&, const GroundingMap&);
virtual bool satisfy(const PatternLinkPtr&);
protected:
Handle _continuation;
};
}
#endif