#ifndef _OPENCOG_PRESENT_LINK_H
#define _OPENCOG_PRESENT_LINK_H
#include <opencog/atoms/core/UnorderedLink.h>
namespace opencog
{
class PresentLink : public UnorderedLink
{
void init(void);
public:
PresentLink(const HandleSeq&&, Type=PRESENT_LINK);
PresentLink(const PresentLink &) = delete;
PresentLink& operator=(const PresentLink &) = delete;
virtual bool is_evaluatable() const { return true; }
virtual bool is_executable() const { return true; }
virtual bool bevaluate(AtomSpace*, bool silent=false);
virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
return ValueCast(evaluate(as, silent));
}
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PresentLink)
#define createPresentLink CREATE_DECL(PresentLink)
}
#endif