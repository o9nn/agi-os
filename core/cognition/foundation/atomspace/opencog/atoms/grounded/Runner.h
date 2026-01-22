#ifndef _OPENCOG_RUNNER_H
#define _OPENCOG_RUNNER_H
#include <opencog/atoms/value/Value.h>
namespace opencog {
class AtomSpace;
class Runner
{
public:
Runner(void) {}
Runner(const Runner&) = delete;
Runner& operator=(const Runner&) = delete;
virtual ~Runner() {}
virtual ValuePtr evaluate(AtomSpace*, const ValuePtr&, bool=false) = 0;
virtual ValuePtr execute(AtomSpace*, const ValuePtr&, bool=false) = 0;
};
}
#endif