#ifndef _OPENCOG_ABSENT_LINK_H
#define _OPENCOG_ABSENT_LINK_H
#include <opencog/atoms/core/UnorderedLink.h>
namespace opencog
{
class AbsentLink : public UnorderedLink
{
void init(void);
public:
AbsentLink(const HandleSeq&&, Type=ABSENT_LINK);
AbsentLink(const AbsentLink &) = delete;
AbsentLink& operator=(const AbsentLink &) = delete;
virtual bool is_evaluatable() const { return true; }
virtual bool is_executable() const { return true; }
virtual bool bevaluate(AtomSpace*, bool silent=false);
virtual ValuePtr execute(AtomSpace* as, bool silent=false) {
return ValueCast(evaluate(as, silent));
}
static Handle factory(const Handle&);
};
LINK_PTR_DECL(AbsentLink)
#define createAbsentLink CREATE_DECL(AbsentLink)
}
#endif