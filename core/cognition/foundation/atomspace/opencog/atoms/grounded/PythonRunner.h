#ifndef _OPENCOG_PYTHON_RUNNER_H
#define _OPENCOG_PYTHON_RUNNER_H
#include <string>
#include <opencog/atoms/grounded/Runner.h>
namespace opencog
{
class PythonRunner : public Runner
{
std::string _fname;
public:
PythonRunner(const std::string);
PythonRunner(const PythonRunner&) = delete;
PythonRunner& operator=(const PythonRunner&) = delete;
virtual ValuePtr execute(AtomSpace*, const ValuePtr&, bool=false);
virtual ValuePtr evaluate(AtomSpace*, const ValuePtr&, bool=false);
};
}
#endif