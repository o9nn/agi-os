#ifndef MOSES_INTERPRETER_H
#define MOSES_INTERPRETER_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/value/LinkValue.h>
namespace opencog
{
namespace atomese
{
typedef std::vector<double >::size_type value_size;
class Interpreter
{
opencog::Handle _key;
public:
Interpreter(const opencog::Handle &input_table, const int size=0);
opencog::ValuePtr operator()(const opencog::Handle& program);
private:
value_size _problem_data_size;
ValuePtr unwrap_constant(const Handle &handle);
ValuePtr execute(const Type t, const ValueSeq &params);
static value_size extract_output_size(const Handle &program, const Handle &key);
bool is_constant(const Type);
};
}
}
#endif