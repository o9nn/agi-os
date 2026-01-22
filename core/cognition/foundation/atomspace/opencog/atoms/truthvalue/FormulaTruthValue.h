#ifndef _OPENCOG_FORMULA_TRUTH_VALUE_H_
#define _OPENCOG_FORMULA_TRUTH_VALUE_H_
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>
namespace opencog
{
class FormulaTruthValue : public SimpleTruthValue
{
protected:
void init(void);
virtual void update(void) const;
HandleSeq _formula;
AtomSpace* _as;
public:
FormulaTruthValue(const Handle&);
FormulaTruthValue(const Handle&, const Handle&);
FormulaTruthValue(const HandleSeq&&);
virtual ~FormulaTruthValue();
std::string to_string(const std::string&) const;
virtual strength_t get_mean() const;
};
VALUE_PTR_DECL(FormulaTruthValue);
CAST_TV_DECL(FormulaTruthValue);
CREATE_VALUE_DECL(FormulaTruthValue);
}
#endif