#ifndef _OPENCOG_SET_TV_LINK_H
#define _OPENCOG_SET_TV_LINK_H
#include <opencog/atoms/flow/SetValueLink.h>
namespace opencog
{
class SetTVLink : public SetValueLink
{
private:
TruthValuePtr eval_direct(AtomSpace*, bool);
TruthValuePtr make_formula(AtomSpace*, bool);
public:
SetTVLink(const HandleSeq&&, Type=SET_TV_LINK);
SetTVLink(const SetTVLink&) = delete;
SetTVLink& operator=(const SetTVLink&) = delete;
virtual bool is_evaluatable() const { return true; }
virtual TruthValuePtr evaluate(AtomSpace*, bool);
virtual ValuePtr execute(AtomSpace* as, bool silent) {
return ValueCast(evaluate(as, silent));
}
static Handle factory(const Handle&);
};
LINK_PTR_DECL(SetTVLink)
#define createSetTVLink CREATE_DECL(SetTVLink)
}
#endif