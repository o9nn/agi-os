#ifndef _OPENCOG_EXECUTE_THREADED_LINK_H
#define _OPENCOG_EXECUTE_THREADED_LINK_H
#include <thread>
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class AtomSpace;
class ExecuteThreadedLink : public Link
{
protected:
size_t _nthreads;
std::thread _joiner;
public:
ExecuteThreadedLink(const HandleSeq&&, Type=EXECUTE_THREADED_LINK);
ExecuteThreadedLink(const ExecuteThreadedLink&) = delete;
ExecuteThreadedLink& operator=(const ExecuteThreadedLink&) = delete;
virtual ~ExecuteThreadedLink();
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ExecuteThreadedLink)
#define createExecuteThreadedLink CREATE_DECL(ExecuteThreadedLink)
}
#endif