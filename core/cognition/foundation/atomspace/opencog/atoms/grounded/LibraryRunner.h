#ifndef _OPENCOG_LIBRARY_RUNNER_H
#define _OPENCOG_LIBRARY_RUNNER_H
#include <string>
#include <opencog/atoms/grounded/Runner.h>
namespace opencog
{
class LibraryRunner : public Runner
{
std::string _fname;
void* sym;
public:
LibraryRunner(const std::string);
LibraryRunner(const LibraryRunner&) = delete;
LibraryRunner& operator=(const LibraryRunner&) = delete;
virtual ValuePtr execute(AtomSpace*, const ValuePtr&, bool=false);
virtual ValuePtr evaluate(AtomSpace*, const ValuePtr&, bool=false);
};
}
#endif