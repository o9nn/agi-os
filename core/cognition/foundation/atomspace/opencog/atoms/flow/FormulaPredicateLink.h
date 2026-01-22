#ifndef _OPENCOG_FORMULA_PREDICATE_LINK_H
#define _OPENCOG_FORMULA_PREDICATE_LINK_H
#include <opencog/atoms/core/ScopeLink.h>
namespace opencog
{
class FormulaPredicateLink : public ScopeLink
{
protected:
void init();
public:
FormulaPredicateLink(const HandleSeq&&, Type=FORMULA_PREDICATE_LINK);
FormulaPredicateLink(const FormulaPredicateLink &) = delete;
FormulaPredicateLink operator=(const FormulaPredicateLink &) = delete;
TruthValuePtr apply(AtomSpace*, const HandleSeq&, bool);
virtual bool is_evaluatable() const { return true; }
virtual bool is_executable() const { return true; }
virtual TruthValuePtr evaluate(AtomSpace*, bool);
virtual ValuePtr execute(AtomSpace* as, bool silent) {
return ValueCast(evaluate(as, silent));
}
static Handle factory(const Handle&);
};
LINK_PTR_DECL(FormulaPredicateLink)
#define createFormulaPredicateLink CREATE_DECL(FormulaPredicateLink)
}
#endif