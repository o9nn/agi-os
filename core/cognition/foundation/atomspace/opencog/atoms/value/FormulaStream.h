#ifndef _OPENCOG_FORMULA_STREAM_H
#define _OPENCOG_FORMULA_STREAM_H
#include <vector>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
class FormulaStream
: public FloatValue
{
protected:
FormulaStream(Type t) : FloatValue(t) {}
void init(void);
virtual void update() const;
HandleSeq _formula;
AtomSpace* _as;
public:
FormulaStream(const Handle&);
FormulaStream(const HandleSeq&&);
FormulaStream(const ValueSeq&);
virtual ~FormulaStream() {}
virtual std::string to_string(const std::string& indent = "") const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(FormulaStream);
CREATE_VALUE_DECL(FormulaStream);
}
#endif