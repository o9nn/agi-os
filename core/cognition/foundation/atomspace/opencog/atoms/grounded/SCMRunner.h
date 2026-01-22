#ifndef _OPENCOG_SCM_RUNNER_H
#define _OPENCOG_SCM_RUNNER_H
#include <string>
#include <opencog/atoms/grounded/Runner.h>
namespace opencog
{
class SCMRunner : public Runner
{
std::string _fname;
public:
SCMRunner(const std::string);
SCMRunner(const SCMRunner&) = delete;
SCMRunner& operator=(const SCMRunner&) = delete;
virtual ValuePtr execute(AtomSpace*, const ValuePtr&, bool=false);
virtual ValuePtr evaluate(AtomSpace* as, const ValuePtr& args, bool silent=false)
{ return execute(as, args, silent); }
};
}
#endif