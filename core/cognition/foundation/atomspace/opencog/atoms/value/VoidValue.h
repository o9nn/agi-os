#ifndef _OPENCOG_VOIDVALUE_H
#define _OPENCOG_VOIDVALUE_H
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class VoidValue : public Value
{
private:
VoidValue() : Value(VOID_VALUE) {}
public:
virtual ~VoidValue() {}
static const ValuePtr INSTANCE;
virtual std::string to_string(const std::string& indent) const {
return indent + "(VoidValue)";
}
virtual bool operator==(const Value& other) const {
return this == &other;
}
};
template<typename ... Type>
static inline ValuePtr createVoidValue(void)
{ return VoidValue::INSTANCE; }
}
#endif