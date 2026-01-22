#ifndef _OPENCOG_PURE_EXEC_LINK_H
#define _OPENCOG_PURE_EXEC_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class AtomSpace;
class PureExecLink : public Link
{
protected:
public:
PureExecLink(const HandleSeq&&, Type=PURE_EXEC_LINK);
PureExecLink(const PureExecLink&) = delete;
PureExecLink& operator=(const PureExecLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(PureExecLink)
#define createPureExecLink CREATE_DECL(PureExecLink)
}
#endif