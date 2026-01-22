#ifndef _OPENCOG_DONT_EXEC_LINK_H
#define _OPENCOG_DONT_EXEC_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class DontExecLink : public Link
{
public:
DontExecLink(const HandleSeq&&, Type=DONT_EXEC_LINK);
DontExecLink(const DontExecLink&) = delete;
DontExecLink& operator=(const DontExecLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool) { return _outgoing[0]; }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(DontExecLink)
#define createDontExecLink CREATE_DECL(DontExecLink)
}
#endif