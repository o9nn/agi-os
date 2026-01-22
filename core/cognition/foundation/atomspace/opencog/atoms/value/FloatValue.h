#ifndef _OPENCOG_FLOAT_VALUE_H
#define _OPENCOG_FLOAT_VALUE_H
#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class FloatValue
: public Value
{
friend class TransposeColumn;
protected:
mutable std::vector<double> _value;
virtual void update() const {}
FloatValue(Type t) : Value(t) {}
public:
FloatValue(Type t, const std::vector<double>& v) : Value(t), _value(v) {}
FloatValue(double v) : Value(FLOAT_VALUE) { _value.push_back(v); }
FloatValue(const std::vector<double>& v)
: Value(FLOAT_VALUE), _value(v) {}
FloatValue(std::vector<double>&& v)
: Value(FLOAT_VALUE), _value(std::move(v)) {}
virtual ~FloatValue() {}
const std::vector<double>& value() const { update(); return _value; }
size_t size() const { return _value.size(); }
virtual ValuePtr incrementCount(const std::vector<double>&) const;
virtual ValuePtr incrementCount(size_t, double) const;
virtual std::string to_string(const std::string& indent = "") const
{ return to_string(indent, _type); }
std::string to_string(const std::string& indent, Type) const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(FloatValue);
CREATE_VALUE_DECL(FloatValue);
std::vector<double> plus(double, const std::vector<double>&);
std::vector<double> minus(double, const std::vector<double>&);
std::vector<double> minus(const std::vector<double>&, double);
std::vector<double> times(double, const std::vector<double>&);
std::vector<double> divide(double, const std::vector<double>&);
inline
ValuePtr plus(double f, const FloatValuePtr& fvp) {
return createFloatValue(plus(f, fvp->value()));
}
inline
ValuePtr minus(double f, const FloatValuePtr& fvp) {
return createFloatValue(minus(f, fvp->value()));
}
inline
ValuePtr minus(const FloatValuePtr& fvp, double f) {
return createFloatValue(minus(fvp->value(), f));
}
inline
ValuePtr times(double f, const FloatValuePtr& fvp) {
return createFloatValue(times(f, fvp->value()));
}
inline
ValuePtr divide(double f, const FloatValuePtr& fvp) {
return createFloatValue(divide(f, fvp->value()));
}
std::vector<double> plus(const std::vector<double>&, const std::vector<double>&);
std::vector<double> minus(const std::vector<double>&, const std::vector<double>&);
std::vector<double> times(const std::vector<double>&, const std::vector<double>&);
std::vector<double> divide(const std::vector<double>&, const std::vector<double>&);
inline
ValuePtr plus(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
if (fvpa != fvpb)
return createFloatValue(plus(fvpa->value(), fvpb->value()));
auto sample = fvpa->value();
return createFloatValue(plus(sample, fvpb->value()));
}
inline
ValuePtr minus(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
if (fvpa != fvpb)
return createFloatValue(minus(fvpa->value(), fvpb->value()));
auto sample = fvpa->value();
return createFloatValue(minus(sample, fvpb->value()));
}
inline
ValuePtr times(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
if (fvpa != fvpb)
return createFloatValue(times(fvpa->value(), fvpb->value()));
auto sample = fvpa->value();
return createFloatValue(times(sample, fvpb->value()));
}
inline
ValuePtr divide(const FloatValuePtr& fvpa, const FloatValuePtr& fvpb) {
if (fvpa != fvpb)
return createFloatValue(divide(fvpa->value(), fvpb->value()));
auto sample = fvpa->value();
return createFloatValue(divide(sample, fvpb->value()));
}
}
#endif