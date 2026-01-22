#ifndef _OPENCOG_NUMBER_NODE_H
#define _OPENCOG_NUMBER_NODE_H
#include <charconv>
#include <opencog/util/exceptions.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/value/FloatValue.h>
#ifndef __cpp_lib_to_chars
#include <boost/lexical_cast.hpp>
#endif
namespace opencog
{
class NumberNode : public Node
{
private:
static std::string double_to_string(double x)
{
#ifdef __cpp_lib_to_chars
static const size_t buf_size = 30;
char buf[buf_size]{};
std::to_chars_result result = std::to_chars(buf, buf + buf_size,
x, std::chars_format::general, 18);
if (result.ec != std::errc())
throw RuntimeException(TRACE_INFO,
"Error: Failed double_to_string(%g): %s\n",
x, std::make_error_code(result.ec).message().c_str());
return std::string(buf);
#else
return boost::lexical_cast<std::string>(x);
#endif
}
protected:
std::vector<double> _value;
public:
NumberNode(Type, const std::string&&);
public:
NumberNode(const std::string&&);
NumberNode(const std::vector<double>&);
NumberNode(std::vector<double>&&);
NumberNode(const FloatValuePtr&);
NumberNode(const ValuePtr&);
NumberNode(double vvv)
: Node(NUMBER_NODE, double_to_string(vvv))
{ _value.push_back(vvv); }
NumberNode(NumberNode&) = delete;
NumberNode& operator=(const NumberNode&) = delete;
static std::vector<double> to_vector(const std::string&);
static std::string vector_to_json(const std::vector<double>&);
static std::string vector_to_plain(const std::vector<double>&);
static std::string validate(const std::string& str)
{
return vector_to_plain(to_vector(str));
}
size_t size() const { return _value.size(); }
const std::vector<double>& value(void) const { return _value; }
double get_value(void) const { return _value[0]; }
static Handle factory(const Handle&);
};
NODE_PTR_DECL(NumberNode)
#define createNumberNode CREATE_DECL(NumberNode)
inline
ValuePtr plus(double f, const NumberNodePtr& fvp) {
return createFloatValue(std::move(plus(f, fvp->value()))); }
inline
ValuePtr minus(double f, const NumberNodePtr& fvp) {
return createFloatValue(std::move(minus(f, fvp->value()))); }
inline
ValuePtr times(double f, const NumberNodePtr& fvp) {
return createFloatValue(std::move(times(f, fvp->value()))); }
inline
ValuePtr divide(double f, const NumberNodePtr& fvp) {
return createFloatValue(std::move(divide(f, fvp->value()))); }
ValuePtr plus(double, const ValuePtr&, bool silent=false);
ValuePtr minus(double, const ValuePtr&, bool silent=false);
ValuePtr times(double, const ValuePtr&, bool silent=false);
ValuePtr divide(double, const ValuePtr&, bool silent=false);
inline
ValuePtr plus(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(plus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr minus(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(minus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr times(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(times(fvpa->value(), fvpb->value()))); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(divide(fvpa->value(), fvpb->value()))); }
inline
ValuePtr plus(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(plus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr minus(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(minus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr times(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(times(fvpa->value(), fvpb->value()))); }
inline
ValuePtr divide(const FloatValuePtr& fvpa, const NumberNodePtr& fvpb) {
return createFloatValue(std::move(divide(fvpa->value(), fvpb->value()))); }
inline
ValuePtr plus(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
return createFloatValue(std::move(plus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr minus(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
return createFloatValue(std::move(minus(fvpa->value(), fvpb->value()))); }
inline
ValuePtr times(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
return createFloatValue(std::move(times(fvpa->value(), fvpb->value()))); }
inline
ValuePtr divide(const NumberNodePtr& fvpa, const FloatValuePtr& fvpb) {
return createFloatValue(std::move(divide(fvpa->value(), fvpb->value()))); }
ValuePtr plus(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr minus(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr times(const ValuePtr&, const ValuePtr&, bool silent=false);
ValuePtr divide(const ValuePtr&, const ValuePtr&, bool silent=false);
}
#endif