#ifndef _OPENCOG_NUMERIC_FUNCTION_LINK_H
#define _OPENCOG_NUMERIC_FUNCTION_LINK_H
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class NumericFunctionLink : public FunctionLink
{
protected:
void init();
ValuePtr execute_unary(AtomSpace*, bool);
ValuePtr execute_binary(AtomSpace*, bool);
static const std::vector<double>* get_vector(AtomSpace*, bool,
ValuePtr, Type&);
static ValuePtr apply_func(AtomSpace*, bool, const Handle&,
double (*)(double), ValuePtr&);
static ValuePtr apply_func(AtomSpace*, bool, const HandleSeq&,
double (*)(double, double), ValueSeq&);
public:
NumericFunctionLink(const HandleSeq&&, Type);
NumericFunctionLink(const NumericFunctionLink&) = delete;
NumericFunctionLink& operator=(const NumericFunctionLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(NumericFunctionLink)
#define createNumericFunctionLink CREATE_DECL(NumericFunctionLink)
}
#endif